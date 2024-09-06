{-# LANGUAGE DeriveGeneric #-}

module ParseJson
  ( parseConfigFile
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B

data Pomodoro = Pomodoro
  { cycles       
  , skipTimeDurationS
  , workDurationM   
  , sBreakDurationM 
  , lBreakDurationM  
  , volumePercent     :: Int
  , pausedAtStart        
  , pausedAfterWork 
  , pausedAfterSBreak 
  , pausedAfterLBreak :: Bool
  } deriving (Show, Generic)

instance FromJSON Pomodoro

parseConfigFile :: IO (Int, Int, Int, Int, Bool, [(String, Int)], 
                     [(String, Bool)])
parseConfigFile = do
  jsonData <- B.readFile "./resources/config.json"
  let eitherJson = eitherDecode jsonData
  return $ case eitherJson of
    Right pomodoro -> ( (workDurationM    pomodoro) * 60
                      , cycles            pomodoro
                      , skipTimeDurationS pomodoro
                      , volumePercent     pomodoro
                      , pausedAtStart     pomodoro
                      , [("work",   (workDurationM     pomodoro) * 60),
                         ("sBreak", (sBreakDurationM   pomodoro) * 60),
                         ("lBreak", (lBreakDurationM   pomodoro) * 60)]
                      , [("work",   pausedAfterWork   pomodoro),
                         ("sBreak", pausedAfterSBreak pomodoro),
                         ("lBreak", pausedAfterLBreak pomodoro)])
