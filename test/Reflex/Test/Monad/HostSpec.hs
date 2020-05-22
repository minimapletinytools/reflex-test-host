{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Reflex.Test.Monad.HostSpec
  ( spec
  )
where

import           Prelude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       ( fromHUnitTest )
import           Test.HUnit

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Functor.Identity

import           Reflex
import           Reflex.Host.Class
import           Reflex.Test.Monad.Host


type T = SpiderTimeline Global

-- | a very basic test network, simple passes on the input event to its observed outputs
basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Int -> TestGuestT t m (Event t Int))
basic_network ev = return ev

-- | test 'basic_network'
test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $ do
  ins <- newEventWithTriggerRef
  runReflexTestM ins basic_network $ do
    -- get our app's output events and subscribe to them
    oh                               <- subscribeEvent =<< outputs

    -- get our input trigger ref, dereference it, queue it and fire it
    intref                           <- inputTriggerRefs
    mh :: Maybe (EventTrigger T Int) <- liftIO $ readRef intref
    case mh of
      Just h  -> queueEventTrigger $ (h :=> Identity 0)
      Nothing -> error "no subscribers to h"
    a <- fireQueuedEventsAndRead $ sequence =<< readEvent oh

    -- validate results
    liftIO $ a @?= [Just 0]



spec :: Spec
spec = do
  fromHUnitTest test_basic
