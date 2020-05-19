{-# LANGUAGE RecordWildCards #-}

-- TODO rename to SimpleHost
-- |
-- Module:
--   Reflex.Test.Host
-- Description:
--   This module contains reflex host methods for testing without external events

module Reflex.Test.Monad.Host
  ( AppIn(..)
  , AppOut(..)
  , AppFrame(..)
  , getAppFrame
  , tickAppFrame
  , runAppSimple
  , runApp
  , runApp'
  , runAppB
  )
where

import           Prelude

import           Control.Monad
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Maybe            (fromJust)
import           Data.These

import           Reflex
import           Reflex.Host.Class


class MonadReflexTest m inh out where
  inputEventHandles :: m inh -- ^ reads input event handles for use in calls to queueEvent
  queueEventTrigger :: DSum (EventTrigger t) Identity -> m ()
  outputs :: m out

  -- TODO seems like you need to do fireQueuedEventsAndRead all at once :(
  -- this means you don't get postbuild stuff unless you do something hacky like first call to this does postbuild...
  readOutput :: ReadPhase (SpiderHost t) a -> m a
  fireQueuedEvents :: m ()

data AppState t = AppState {
  _appState_queuedEvents :: [DSum (EventTrigger t) Identity] -- ^ events to fire in next 'FireCommand'
  , _appState_fire :: FireCommand t (SpiderHost t) -- ^ 'FireCommand' to fire events and run next frame

  -- TODO DELETE won't work, each call to readPhase fires events again
  , _appState_readPhase :: forall a. ReadPhase (SpiderHost t) a -> SpiderHost t a -- ^ method to execute 'ReadPhase' from previous 'FireCommand' or from 'PostBuild' if no previously fired events
}

newtype ReflexTestM inh out t a = ReflexTestM { unReflexTestM :: (inh, out) -> AppState t -> SpiderHost t (AppState t, a) }

instance Functor (ReflexTestM inh out t) where
  fmap f ma = ReflexTestM $ \io as -> over _2 (fmap f) $ unReflexTestM ma io as

instance Applicative (ReflexTestM inh out t) where
  pure a = ReflexTestM $ \_ as -> return (as, a)
  liftA2 f ma mb = ReflexTestM $ \io as0 -> r where
    (as1, a) = unReflexTestM ma io as0
    (as2, b) = unReflexTestM mb io as1
    r = return (as2, f a b)

instance Monad (ReflexTestM inh out t) where
  (>>=) ma f = ReflexTestM $ \io as0 -> r where
    (as1, a) = unReflexTestM ma io as0
    r = unReflexTestM (f a) io as1

instance MonadReflexTest (ReflexTestM inh in out t m) inh out where
  inputEventHandles = ReflexTestM $ \(inh,_) as -> return (as, inh)
  queueEventTrigger evt = ReflexTestM $ \_ as -> return (as { _appState_queuedEvents = evt : _appState_queuedEvents as}, ())
  outputs = ReflexTestM $ \(_,out) as -> return (as, out)
  readOutput rp = ReflexTestM $ \_ as -> fmap (as,) $ _appState_readPhase as rp
  fireQueuedEvents = ReflexTestM $ \_ as -> return (as { _appState_readPhase = _appState_fire as (_appState_queuedEvents as) }, ())


runReflexTestM ::
  ReflextTestM inh in out t m a
  -> (inh, in) -- ^ make sure inh match inputs, i.e. return values of newEventWithTriggerRef
  -> (in -> TriggerEventT (PostBuildT (PerformEventT t m out)))
  -> IO a
runReflexTestM rtm (inputH, input) app = do
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
  events <- liftIO newChan
  (output, fc@(FireCommand fire)) <- do
    hostPerformEventT $
      flip runPostBuildT postBuild $
        flip runTriggerEventT events $
          app input

  mPostBuildTrigger <- readRef postBuildTriggerRef
  rp0 <- case mPostBuildTrigger of
    Nothing -> error "could not create PostBuild trigger ref"
    Just postBuildTrigger -> fire [postBuildTrigger :=> Identity ()]


  -- TODO figure out how to do this
  -- correct solution is to implement non-blocking variant of TriggerEventT
  -- and then pass as part of AppState such that each call to readPhase will fire any trigger events
  -- another option is just to start a thread and output warnings anytime triggerEvs are created
  --triggerEvs <- liftIO $ readChan events

  unReflexTestM rtm (inputH, output) (AppState [] fc rp0)




class ReflexTestApp inh in out app t m where
  testApp :: in -> TriggerEventT (PostBuildT (PerformEventT t m out))
  makeInputs :: SpiderHost t (inh, in)


runReflexTestApp :: (ReflexTestApp inh in out app t m a) =>
