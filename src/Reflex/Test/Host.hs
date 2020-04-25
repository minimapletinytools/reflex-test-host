{-# LANGUAGE RecordWildCards #-}

module Reflex.Test.Host
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

data AppIn t b e = AppIn
    { _appIn_behavior :: Behavior t b
    , _appIn_event    :: Event t e
    }

data AppOut t b e = AppOut
    { _appOut_behavior :: Behavior t b
    , _appOut_event    :: Event t e
    }

data AppFrame t bIn eIn bOut eOut m = AppFrame
    { _appFrame_readPhase :: ReadPhase m (bOut, Maybe eOut)
    , _appFrame_pulseB :: EventTrigger t bIn
    , _appFrame_pulseE :: EventTrigger t eIn
    , _appFrame_fire :: forall a .
  [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m [a]
    }

-- | make an 'AppFrame' that takes an input behavior and event and returns an
-- output behavior and event.
getAppFrame
  :: forall t bIn eIn bOut eOut m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
  -> bIn
  -> m (AppFrame t bIn eIn bOut eOut m)
getAppFrame app b0 = do
  (appInHoldE, pulseHoldTriggerRef ) <- newEventWithTriggerRef
  (appInE    , pulseEventTriggerRef) <- newEventWithTriggerRef
  appInB                             <- hold b0 appInHoldE
  (out :: AppOut t bOut eOut, FireCommand fire) <-
    hostPerformEventT $ app $ AppIn { _appIn_event    = appInE
                                    , _appIn_behavior = appInB
                                    }
  hnd :: EventHandle t eOut <- subscribeEvent (_appOut_event out)
  mpulseB                   <- readRef pulseHoldTriggerRef
  mpulseE                   <- readRef pulseEventTriggerRef
  let readPhase = do
        b      <- sample (_appOut_behavior out)
        frames <- sequence =<< readEvent hnd
        return (b, frames)
  return AppFrame { _appFrame_readPhase = readPhase
                  , _appFrame_pulseB    = fromJust mpulseB
                  , _appFrame_pulseE    = fromJust mpulseE
                  , _appFrame_fire      = fire
                  }

-- | Tick an app frame once with optional input behavior and event values.
-- Returns behaviors and events from the app's output for each frame that run
-- for the input (i.e. 'runWithAdjust' and 'performEvent' may cause several
-- frames to run for each input)
--
-- N.B. output behavior will not reflect changes that happen during its frame
-- i.e. this is analogous to 'tag' and 'tagPromptlyDyn'. If you need the most
-- recent behavior value you can always call 'tickAppFrame' with 'Nothing' as
-- input
tickAppFrame
  :: (t ~ SpiderTimeline Global)
  => AppFrame t bIn eIn bOut eOut m
  -> Maybe (These bIn eIn)
  -> m [(bOut, Maybe eOut)]
tickAppFrame AppFrame {..} input = case input of
  Nothing -> fire [] $ readPhase
  Just i  -> case i of
    This b' -> fire [pulseB :=> Identity b'] $ readPhase
    That e' -> fire [pulseE :=> Identity e'] $ readPhase
    These b' e' ->
      fire [pulseB :=> Identity b', pulseE :=> Identity e'] $ readPhase
 where
  fire      = _appFrame_fire
  readPhase = _appFrame_readPhase
  pulseB    = _appFrame_pulseB
  pulseE    = _appFrame_pulseE

-- | calls 'tickAppFrame' for each input in a list and returns collected results
-- see comments for 'tickAppFrame'
runApp
  :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
  -> bIn
  -> [Maybe (These bIn eIn)]
  -> IO [[(bOut, Maybe eOut)]]
runApp app b0 input = runSpiderHost $ do
  appFrame <- getAppFrame app b0
  forM input $ tickAppFrame appFrame

-- | run an app with provided list of input events returns list of results for
-- each input. Each result is a list of events from the app's output for each
-- frame that run for the input.
-- see comments for 'tickAppFrame'
runAppSimple
  :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t eIn -> PerformEventT t m (Event t eOut))
  -> [eIn]
  -> IO [[Maybe eOut]]
runAppSimple app input = runApp' app (map Just input)

-- | same as runAppSimple except input event for each frame is optional
-- see comments for 'tickAppFrame'
runApp'
  :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t eIn -> PerformEventT t m (Event t eOut))
  -> [Maybe eIn]
  -> IO [[Maybe eOut]]
runApp' app input = do
  let app' = fmap (AppOut (pure ())) . app
  map (map snd) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

-- | same as runApp' except only returns sampled output behavior
-- see comments for 'tickAppFrame'
runAppB
  :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t eIn -> PerformEventT t m (Behavior t bOut))
  -> [Maybe eIn]
  -> IO [[bOut]]
runAppB app input = do
  let app' = fmap (flip AppOut never) . app
  map (map fst) <$> runApp (app' . _appIn_event) () (map (fmap That) input)
