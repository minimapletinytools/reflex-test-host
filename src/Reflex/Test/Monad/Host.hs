{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}

-- TODO rename to SimpleHost
-- |
-- Module:
--   Reflex.Test.Host
-- Description:
--   This module contains reflex host methods for testing without external events

module Reflex.Test.Monad.Host
  ( ReflexHostT
  , TestGuestConstraints
  , MonadReflexTest(..)
  , AppState(..)
  , ReflexTestM(..)
  , runReflexTestM
  )
where

import           Prelude


import           Control.Concurrent.Chan
import           Control.Monad.IO.Class

import qualified Control.Applicative            ( liftA2 )
import           Control.Monad.Fix
import           Control.Monad.Ref
import           Control.Monad.Trans.Class
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Kind

import           Reflex
import           Reflex.Host.Class

type ReflexHostT t (m :: Type -> Type)
  = TriggerEventT t (PostBuildT t (PerformEventT t m))

type TestGuestConstraints t (m :: Type -> Type)
  = ( MonadReflexHost t m
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

-- |
class MonadReflexTest t m | m -> t  where
  -- | since event subscriptions also happen within the monad, input triggers created via 'newEventWithTriggerRef' may be stuck in the 'Nothing' state as there are no listeners yet
  -- therefore it's necessary to pass in IORefs to the EventTriggers, thus the name of this type
  -- in practice, this will likely be a record containing many trigger refs and the monad user must deref them all
  -- TODO is there a better way to do this? Perhaps 'ReflexTestApp' can contain methods to help convert 'InputTriggerRefs m' to various 'EventTrigger t'
  type InputTriggerRefs m :: Type
  -- | in practice, this will likely be a record containing events and behaviors to build a 'ReadPhase' that is passed into 'fireQueuedEventsAndRead'
  type OutputEvents m :: Type
  type InnerMonad m :: Type -> Type
  -- | see comments for 'InputTriggerRefs'
  inputTriggerRefs :: m (InputTriggerRefs m)
  -- | all queued triggers will fire simultaneous on the next execution of 'fireQueuedEventsAndRead'
  queueEventTrigger :: DSum (EventTrigger t) Identity -> m ()
  -- | see comments for 'OutputEvents'
  outputs :: m (OutputEvents m)
  -- | fire all queued events and run a ReadPhase to produce results from the execution frames
  -- readphase takes place in the inner monad
  fireQueuedEventsAndRead :: ReadPhase (InnerMonad m) a -> m [a]

-- m is 'InnerMonad' from above
data AppState t m = AppState
    { _appState_queuedEvents :: [DSum (EventTrigger t) Identity] -- ^ events to fire in next 'FireCommand'
    -- ^ 'FireCommand' to fire events and run next frame
    , _appState_fire         :: FireCommand t m -- ^ 'FireCommand' to fire events and run next frame
    }

-- | implementation of 'MonadReflexTest'
newtype ReflexTestM t mintref out m a = ReflexTestM { unReflexTestM :: (mintref, out) -> AppState t m -> m (AppState t m, a) }

instance MonadTrans (ReflexTestM t mintref out) where
  lift m = ReflexTestM $ \_ as -> fmap (\a -> (as, a)) m

instance (Functor m) => Functor (ReflexTestM t mintref out m) where
  fmap f ma = ReflexTestM
    $ \io as -> fmap (\(as', a) -> (as', f a)) $ unReflexTestM ma io as

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

instance (MonadSubscribeEvent t m) => MonadSubscribeEvent t (ReflexTestM t mintref out m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadIO m) => MonadIO (ReflexTestM t mintref out m) where
  liftIO = lift . liftIO

instance (Monad m) => MonadReflexTest t (ReflexTestM t mintref out m) where
  type InputTriggerRefs (ReflexTestM t mintref out m) = mintref
  type OutputEvents (ReflexTestM t mintref out m) = out
  type InnerMonad (ReflexTestM t mintref out m) = m
  inputTriggerRefs = ReflexTestM $ \(mintref, _) as -> return (as, mintref)
  queueEventTrigger evt = ReflexTestM $ \_ as -> return
    (as { _appState_queuedEvents = evt : _appState_queuedEvents as }, ())
  outputs = ReflexTestM $ \(_, out) as -> return (as, out)
  fireQueuedEventsAndRead rp = ReflexTestM $ \_ as ->
    fmap (as, )
      $ (runFireCommand $ _appState_fire as) (_appState_queuedEvents as) rp

runReflexTestM
  :: forall mintref inev out t m a
   . (TestGuestConstraints t m)
  => (inev, mintref) -- ^ make sure mintref match inev, i.e. return values of newEventWithTriggerRef
  -> (inev -> ReflexHostT t m out) -- ^ network to test
  -> ReflexTestM t mintref out m a -- ^ test monad to run
  -> m ()
runReflexTestM (input, inputTRefs) app rtm = do
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

  events                           <- liftIO newChan
  (output, fc@(FireCommand fire))  <- do
    hostPerformEventT
      $ flip runPostBuildT    postBuild
      $ flip runTriggerEventT events
      $ app input

  -- handle post build
  -- TODO consider adding some way to test 'PostBuild' results
  mPostBuildTrigger <- readRef postBuildTriggerRef
  _                 <- case mPostBuildTrigger of
    Nothing -> return [()] -- no subscribers
    Just postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()


  -- TODO maybe find a way to handle trigger events
  -- one solution is to implement non-blocking variant of TriggerEventT
  -- and then pass as part of AppState such that each call to readPhase will fire any trigger events
  -- another option is just to start a thread and output warnings anytime triggerEvs are created
  --triggerEvs <- liftIO $ readChan events

  -- run the test monad
  unReflexTestM rtm (inputTRefs, output) (AppState [] fc)

  return ()



-- | class to help bind network and types to a 'ReflexTestM'
-- TODO write an example using this
class ReflexTestApp app t m | app -> t m where
  data AppInputTriggerRefs app :: Type
  data AppInputEvents app :: Type
  data AppOutput app :: Type
  getApp :: AppInputEvents app -> ReflexHostT t m (AppOutput app)
  makeInputs :: m (AppInputEvents app, AppInputTriggerRefs app)
-- TODO to simplify MonadReflexTest interface, maybe we could do something like
-- subscribeEventsAndReturnTriggers :: m (AppInputTriggers app)
-- and then change (InputTriggerRefs (ReflexTestM ...)) to just (InputTrigger (ReflexTestM ...))

runReflexTestApp
  :: (ReflexTestApp app t m, TestGuestConstraints t m)
  => ReflexTestM t (AppInputTriggerRefs app) (AppOutput app) m ()
  -> m ()
runReflexTestApp rtm = do
  i <- makeInputs
  runReflexTestM i getApp rtm
