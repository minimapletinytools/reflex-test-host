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
  , MonadRef m
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  --, PrimMonad (HostFrame t)
  , MonadIO m
  , MonadFix m
  )

class MonadReflexTest t m | m -> t  where
  type InputTriggerRefs m :: *
  type OutputEvents m :: *
  type InnerMonad m :: * -> *
  inputEventHandles :: m (InputTriggerRefs m) -- ^ reads input event handles for use in calls to queueEvent
  queueEventTrigger :: DSum (EventTrigger t) Identity -> m ()
  outputs :: m (OutputEvents m)
  -- readphase takes place in the inner monad
  fireQueuedEventsAndRead :: ReadPhase (InnerMonad m) a -> m [a]
  -- TODO consider adding "firePostBuildAndRead" which isn't great because it's a required setup step for the user and makes monads less composable
  -- another option is to hack it so first call to fireQUeuedEventsAndRead does PostBuild stuff...

-- m is inner monad
data AppState t m = AppState
    { _appState_queuedEvents :: [DSum (EventTrigger t) Identity] -- ^ events to fire in next 'FireCommand'
    -- ^ 'FireCommand' to fire events and run next frame
    , _appState_fire         :: FireCommand t m -- ^ 'FireCommand' to fire events and run next frame
    }

newtype ReflexTestM t mintref out m a = ReflexTestM { unReflexTestM :: (mintref, out) -> AppState t m -> m (AppState t m, a) }

instance MonadTrans (ReflexTestM t mintref out) where
  lift m = ReflexTestM $ \_ as -> fmap (\a -> (as,a)) m

instance (Functor m) => Functor (ReflexTestM t mintref out m) where
  fmap f ma = ReflexTestM $ \io as -> fmap (\(as',a) -> (as', f a)) $ unReflexTestM ma io as

instance (Applicative m, Monad m) => Applicative (ReflexTestM t mintref out m) where
  pure a = ReflexTestM $ \_ as -> pure (as, a)
  liftA2 f ma mb = ReflexTestM $ \io as0 -> do
    -- TODO rewrite this using liftA2 and drop the Monad constraint
    (as1, a) <- unReflexTestM ma io as0
    (as2, b) <- unReflexTestM mb io as1
    pure (as2, f a b)

instance (Monad m) => Monad (ReflexTestM t mintref out m) where
  (>>=) ma f = ReflexTestM $ \io as0 -> do
    (as1, a) <- unReflexTestM ma io as0
    unReflexTestM (f a) io as1

instance (Monad m) => MonadReflexTest t (ReflexTestM t mintref out m) where
  type InputTriggerRefs (ReflexTestM t mintref out m) = mintref
  type OutputEvents (ReflexTestM t mintref out m) = out
  type InnerMonad (ReflexTestM t mintref out m) = m
  inputEventHandles = ReflexTestM $ \(mintref,_) as -> return (as, mintref)
  queueEventTrigger evt = ReflexTestM $ \_ as -> return (as { _appState_queuedEvents = evt : _appState_queuedEvents as}, ())
  outputs = ReflexTestM $ \(_,out) as -> return (as, out)
  fireQueuedEventsAndRead rp = ReflexTestM $ \_ as -> fmap (as,) $ (runFireCommand $ _appState_fire as) (_appState_queuedEvents as) rp


instance (MonadSubscribeEvent t m) => MonadSubscribeEvent t (ReflexTestM t mintref out m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadIO m) => MonadIO (ReflexTestM t mintref out m) where
  liftIO = lift . liftIO

runReflexTestM :: forall mintref inev out t m a. (TestGuestConstraints t m)
  -- TODO rename mintref to intref or something
  => (inev, mintref) -- ^ make sure mintref match inputs, i.e. return values of newEventWithTriggerRef
  -> (inev -> TriggerEventT t (PostBuildT t (PerformEventT t m)) out)
  -> ReflexTestM t mintref out m a
  -> m ()
runReflexTestM (input, inputH) app rtm = do
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
  -- one solution is to implement non-blocking variant of TriggerEventT
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
