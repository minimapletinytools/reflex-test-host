{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}

-- TODO rename to SimpleHost
-- |
-- Module:
--   Reflex.Test.Host
-- Description:
--   This module contains reflex host methods for testing without external events

module Reflex.Test.Monad.Host
  (
    TestGuestConstraints
    , MonadReflexTest(..)
    , AppState(..)
    , ReflexTestM(..)
    , runReflexTestM
  )
where

import           Prelude


import           Control.Concurrent.Chan   (newChan, readChan, writeChan)
import           Control.Monad.IO.Class

import qualified Control.Applicative       (liftA2)
import           Control.Lens              (over, _2)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Ref
import           Control.Monad.Trans.Class
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Maybe                (fromJust)
import           Data.These

import           Reflex
import           Reflex.Host.Class
import           Reflex.Spider.Internal    (HasSpiderTimeline)

type ReflexHostT t (m :: * -> *)  = TriggerEventT t (PostBuildT t (PerformEventT t m))

type TestGuestConstraints t (m :: * -> *) =
  ( MonadReflexHost t m
  , MonadHold t m
  , MonadSample t m
  , Ref m ~ Ref IO
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  --, PrimMonad (HostFrame t)
  , MonadIO m
  , MonadFix m
  )


-- TODO switch inh and out to type family
-- TODO rename inh to minh because event triggers will almost certainly be wrapped in a maybe...
class MonadReflexTest t inh out m m' | m -> m' t inh out where
  inputEventHandles :: m inh -- ^ reads input event handles for use in calls to queueEvent
  queueEventTrigger :: DSum (EventTrigger t) Identity -> m ()
  outputs :: m out
  -- readphase takes place in the inner monad
  fireQueuedEventsAndRead :: ReadPhase m' a -> m [a]
  -- TODO consider adding "firePostBuildAndRead" which isn't great because it's a required setup step for the user and makes monads less composable
  -- another option is to hack it so first call to fireQUeuedEventsAndRead does PostBuild stuff...

-- m is inner monad
data AppState t m = AppState
    { _appState_queuedEvents :: [DSum (EventTrigger t) Identity] -- ^ events to fire in next 'FireCommand'
    -- ^ 'FireCommand' to fire events and run next frame
    , _appState_fire         :: FireCommand t m -- ^ 'FireCommand' to fire events and run next frame
    }

newtype ReflexTestM t inh out m a = ReflexTestM { unReflexTestM :: (inh, out) -> AppState t m -> m (AppState t m, a) }

instance MonadTrans (ReflexTestM t inh out) where
  lift m = ReflexTestM $ \_ as -> fmap (\a -> (as,a)) m

instance (Functor m) => Functor (ReflexTestM t inh out m) where
  fmap f ma = ReflexTestM $ \io as -> fmap (\(as',a) -> (as', f a)) $ unReflexTestM ma io as

instance (Applicative m, Monad m) => Applicative (ReflexTestM t inh out m) where
  pure a = ReflexTestM $ \_ as -> pure (as, a)
  liftA2 f ma mb = ReflexTestM $ \io as0 -> do
    -- TODO rewrite this using liftA2 and drop the Monad constraint
    (as1, a) <- unReflexTestM ma io as0
    (as2, b) <- unReflexTestM mb io as1
    pure (as2, f a b)

instance (Monad m) => Monad (ReflexTestM t inh out m) where
  (>>=) ma f = ReflexTestM $ \io as0 -> do
    (as1, a) <- unReflexTestM ma io as0
    unReflexTestM (f a) io as1

instance (Monad m') => MonadReflexTest t inh out (ReflexTestM t inh out m') m' where
  inputEventHandles = ReflexTestM $ \(inh,_) as -> return (as, inh)
  queueEventTrigger evt = ReflexTestM $ \_ as -> return (as { _appState_queuedEvents = evt : _appState_queuedEvents as}, ())
  outputs = ReflexTestM $ \(_,out) as -> return (as, out)
  fireQueuedEventsAndRead rp = ReflexTestM $ \_ as -> fmap (as,) $ (runFireCommand $ _appState_fire as) (_appState_queuedEvents as) rp


instance (MonadSubscribeEvent t m) => MonadSubscribeEvent t (ReflexTestM t inh out m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadIO m) => MonadIO (ReflexTestM t inh out m) where
  liftIO = lift . liftIO

-- TODO make general version work
{-runReflexTestM ::
  (inh, inev) -- ^ make sure inh match inputs, i.e. return values of newEventWithTriggerRef
  -> (forall t m. (TestGuestConstraints t m) => inev -> TriggerEventT t (PostBuildT t (PerformEventT t m)) out)
  -> (forall t m. ReflexTestM t inh out m a)
  -> IO ()
runReflexTestM (inputH, input) app rtm =  withSpiderTimeline $ runSpiderHostForTimeline $ do
-}

runReflexTestM ::
  (inh, inev) -- ^ make sure inh match inputs, i.e. return values of newEventWithTriggerRef
  -> (inev -> ReflexHostT (SpiderTimeline Global) (SpiderHost Global) out)
  -> ReflexTestM (SpiderTimeline Global) inh out (SpiderHost Global) a
  -> IO ()
runReflexTestM (inputH, input) app rtm = runSpiderHost $ do
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

  events <- liftIO newChan
  (output, fc@(FireCommand fire)) <- do
    hostPerformEventT $
      flip runPostBuildT postBuild $
        flip runTriggerEventT events $
          app input

  mPostBuildTrigger <- readRef postBuildTriggerRef
  _ <- case mPostBuildTrigger of
    Nothing               -> return [()] -- no subscribers
    Just postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()


  -- TODO figure out how to do this
  -- correct solution is to implement non-blocking variant of TriggerEventT
  -- and then pass as part of AppState such that each call to readPhase will fire any trigger events
  -- another option is just to start a thread and output warnings anytime triggerEvs are created
  --triggerEvs <- liftIO $ readChan events

  unReflexTestM rtm (inputH, output) (AppState [] fc)
  return ()


{-
class ReflexTestApp app t m where
  type AppInputHandles app t :: *
  type AppInputEvents app t :: *
  type AppOutput app t :: *
  getApp :: AppInputEvents app t -> TriggerEventT t (PostBuildT t (PerformEventT t m)) (AppOutput app t)
  makeInputs :: m (AppInputHandles app t, AppInputEvents app t)

runReflexTestApp ::
  (forall t m. (ReflexTestApp app t m) => ReflexTestM t (AppInputHandles app t) (AppOutput app t) m ())
  -> IO ()
runReflexTestApp rtm = do
  i <- makeInputs
  runReflexTestM rtm i getApp
-}
