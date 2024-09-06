{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import MakeOutput as MP
import CalculateTime as CT
import ParseJson  as JP

import Control.Monad (void, forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T

import Brick.BChan
import Brick.Main (App(..), customMainWithDefaultVty, halt)
import Brick.Types (Widget, EventM, BrickEvent(..))
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (str)

data CustomEvent = Tick

data PomodoroState n = PomodoroState
    { _t
    , _currentCycle
    , _maxCycle
    , _skipTimeDuration
    , _volume                  :: Int 
    , _settings
    , _paused                  :: Bool
    , _intervalType            :: String
    , _intervalDurations       :: [(String, Int)]
    , _pausedAfterIntervalType :: [(String, Bool)]
    }

makeLenses ''PomodoroState

drawUI :: PomodoroState () -> [Widget ()]
drawUI p = [ui]
  where
    ui = center $ str $
      MP.showUnicodeTime
        (_t p)
        (_currentCycle p)
        (_maxCycle p)
        (_intervalType p)
        (_paused p)

resetPomodoro :: EventM () (PomodoroState ()) ()
resetPomodoro = do 
  intervalDurations' <- use intervalDurations
  t            .= CT.getLookupInt "work" intervalDurations'
  currentCycle .= 1
  intervalType .= "work"

startChangeTime :: String -> EventM () (PomodoroState ()) ()
startChangeTime changeType = do
  t'                 <- use t
  skipTimeDuration'  <- use skipTimeDuration
  currentCycle'      <- use currentCycle
  maxCycle'          <- use maxCycle
  intervalType'      <- use intervalType
  intervalDurations' <- use intervalDurations
  let timeAdjustment = case changeType of
        "tick"         -> (-1)
        "jump forward" -> (-skipTimeDuration')
        "jump back"    -> skipTimeDuration'
      (processedTime, newCurrentCycle, newIntervalType) = 
        CT.changeTime t' timeAdjustment currentCycle' maxCycle' intervalType' 
                      intervalDurations' 
  t            .= processedTime
  currentCycle .= newCurrentCycle
  intervalType .= newIntervalType

startSkipTime :: String -> EventM () (PomodoroState ()) ()
startSkipTime skipType = do
  t'                 <- use t
  currentCycle'      <- use currentCycle
  maxCycle'          <- use maxCycle
  intervalType'      <- use intervalType
  intervalDurations' <- use intervalDurations 
  let (newCycle, newIntervalType) =
        CT.skipTime t' currentCycle' maxCycle' intervalType'
                    intervalDurations' skipType
  currentCycle .= newCycle
  intervalType .= newIntervalType
  t            .= CT.getLookupInt newIntervalType intervalDurations'

doTick :: EventM () (PomodoroState ()) ()
doTick = do
  paused' <- use paused
  when (paused' == False) $ do
    startChangeTime "tick"
    startPlaySound
    startPauseAfterWork

startPlaySound :: EventM () (PomodoroState ()) ()
startPlaySound = do
  intervalType' <- use intervalType
  t'            <- use t
  when (t' == 0) $ do     
    intervalType' <- use intervalType
    volume'       <- use volume
    (liftIO $ forkIO $ playSound t' intervalType' volume') >> return ()

startPauseAfterWork :: EventM () (PomodoroState ()) ()
startPauseAfterWork = do 
  t'                       <- use t
  currentCycle'            <- use currentCycle
  intervalType'            <- use intervalType
  intervalDurations'       <- use intervalDurations
  pausedAfterIntervalType' <- use pausedAfterIntervalType
  let newPaused = 
        CT.mapPauseAfterInterval t' currentCycle' intervalType' intervalDurations' 
                                 pausedAfterIntervalType'
  paused .= newPaused
    
pomodoroEvent :: BrickEvent () CustomEvent -> EventM () (PomodoroState ()) ()
pomodoroEvent (VtyEvent e) = 
         case e of
           V.EvKey (V.KChar 'r') [] -> resetPomodoro
           V.EvKey (V.KChar 'p') [] -> paused   %= not
           V.EvKey (V.KChar 's') [] -> settings %= not
           V.EvKey (V.KChar '+') [] -> startChangeTime "jump forward"
           V.EvKey (V.KChar '-') [] -> startChangeTime "jump back"
           V.EvKey (V.KChar '>') [] -> startSkipTime   "forward"
           V.EvKey (V.KChar '<') [] -> startSkipTime   "back"
           V.EvKey (V.KChar 'q') [] -> halt
           _                        -> return ()
pomodoroEvent (AppEvent Tick) = doTick

pomodoroApp :: App (PomodoroState ()) CustomEvent ()
pomodoroApp =
  App { appDraw         = drawUI
      , appChooseCursor = M.showFirstCursor
      , appHandleEvent  = pomodoroEvent
      , appStartEvent   = return ()
      , appAttrMap      = const (A.attrMap V.defAttr [])
      }

startTick :: IO (BChan CustomEvent)
startTick = do 
  chan <- newBChan 10 
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000
  return chan

startPomodoroTimer :: BChan CustomEvent -> IO ()
startPomodoroTimer chan = do
  (t', maxCycle',  skipTimeDuration', volume', paused', intervalDurations', 
    pausedAfterIntervalType') <- JP.parseConfigFile
  let initialState = 
        PomodoroState t' 1 maxCycle' skipTimeDuration' volume' False paused' "work"
                      intervalDurations' pausedAfterIntervalType'
  void $ customMainWithDefaultVty (Just chan) pomodoroApp initialState

quitPomodoro :: IO ()
quitPomodoro = do 
  vty <- mkVty V.defaultConfig
  V.shutdown vty

main :: IO ()
main = do
  chan <- startTick
  startPomodoroTimer chan
  quitPomodoro
