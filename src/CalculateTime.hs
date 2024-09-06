module CalculateTime
    ( getLookupInt,
      changeTime,
      skipTime,
      mapPauseAfterInterval,
    ) where

getLookupInt :: String -> [(String, Int)] -> Int
getLookupInt intervalType pomodoroMap =
  let maybeLookupInt = lookup intervalType pomodoroMap
  in case maybeLookupInt of
    Just duration -> duration
    Nothing       -> 0

getLookupBool :: String -> [(String, Bool)] -> Bool
getLookupBool intervalType pomodoroMap =
  let maybeLookupBool = lookup intervalType pomodoroMap
  in case maybeLookupBool of
    Just duration -> duration
    Nothing       -> False

getNextIntervalType :: String -> Int -> Int -> String
getNextIntervalType intervalType currentCycle maxCycle = 
  case intervalType of
    "work"   -> if currentCycle == maxCycle
                  then "lBreak"
                  else "sBreak"
    "sBreak" -> "work"
    "lBreak" -> "work"

getNextCycle :: String -> Int -> Int
getNextCycle intervalType currentCycle = 
  case intervalType of
    "work"   -> currentCycle
    "sBreak" -> currentCycle + 1
    "lBreak" -> 1

getPreviousIntervalType :: String -> Int -> String
getPreviousIntervalType intervalType currentCycle =
  case intervalType of
    "work"   -> if currentCycle == 1
                  then "lBreak"
                  else "sBreak"
    "sBreak" -> "work"
    "lBreak" -> "work"

getPreviousCycle :: String -> Int -> Int -> Int
getPreviousCycle intervalType currentCycle maxCycle =
  case intervalType of
    "work" -> if currentCycle == 1
                then maxCycle
                else currentCycle - 1
    "sBreak" -> currentCycle
    "lBreak" -> currentCycle

skipTime :: Int -> Int -> Int -> String -> [(String, Int)] -> String -> 
            (Int, String)
skipTime t currentCycle maxCycle intervalType intervalDurations skipType = 
  case skipType of
         "forward" -> (getNextCycle intervalType currentCycle,
                       getNextIntervalType intervalType currentCycle 
                         maxCycle)
         "back"    -> if ((getLookupInt intervalType intervalDurations) - t)
                          < 6 
                         then (getPreviousCycle intervalType currentCycle maxCycle,
                               getPreviousIntervalType intervalType currentCycle)
                         else (currentCycle, intervalType)

changeTime :: Int -> Int -> Int -> Int -> String -> [(String, Int)] -> 
              (Int, Int, String)
changeTime t skipTimeDuration currentCycle maxCycle intervalType 
           intervalDurations = do
    let skippedTime = t + skipTimeDuration
    if skippedTime < 0
      then  
        let newIntervalType = getNextIntervalType intervalType currentCycle 
                              maxCycle
            newTime = getLookupInt newIntervalType intervalDurations 
            newCurrentCycle = getNextCycle intervalType currentCycle
        in (newTime, newCurrentCycle, newIntervalType)
    else 
        if skippedTime > getLookupInt intervalType intervalDurations
          then 
              let (newCurrentCycle, newIntervalType) = 
                    skipTime t currentCycle maxCycle intervalType 
                    intervalDurations "back"
                  newTime = if newIntervalType /= intervalType
                              then skipTimeDuration
                              else getLookupInt newIntervalType 
                                   intervalDurations
              in (newTime, newCurrentCycle, newIntervalType) 
        else (skippedTime, currentCycle, intervalType)

mapPauseAfterInterval :: Int -> Int -> String -> [(String, Int)] -> 
                         [(String, Bool)] -> Bool
mapPauseAfterInterval t currentCycle intervalType intervalDurations 
                      pausedAfterInterval = 
  if (t == (getLookupInt intervalType intervalDurations))
  then let previousIntervalType = getPreviousIntervalType intervalType currentCycle
       in getLookupBool previousIntervalType pausedAfterInterval
  else False
