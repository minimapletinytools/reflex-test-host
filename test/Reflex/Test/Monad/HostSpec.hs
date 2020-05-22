{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Reflex.Test.Monad.HostSpec
  ( spec
  )
where

import           Prelude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Host.Class
import           Reflex.Test.Monad.Host

import           Control.Monad            (forM_)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Maybe


type T = SpiderTimeline Global

basic_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Int -> TriggerEventT t (PostBuildT t (PerformEventT t m)) (Event t Int))
basic_network ev = return ev

test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $ do
  (inev, intref) <- newEventWithTriggerRef
  minh <- readRef intref
  let
    testm :: ReflexTestM T (Maybe (EventTrigger T Int)) (Event T Int) (SpiderHost Global) ()
    testm = do
      o <- outputs
      oh <- subscribeEvent o
      mh :: Maybe (EventTrigger T Int) <- inputEventHandles
      case mh of
        Just h  -> queueEventTrigger $ (h :=> Identity 0)
        Nothing -> error "no subscribers to h"
      a <- fireQueuedEventsAndRead $ sequence =<< readEvent oh
      liftIO $ a @?= [Just 0]

  runReflexTestM (minh, inev) basic_network testm



spec :: Spec
spec = do
  fromHUnitTest test_basic