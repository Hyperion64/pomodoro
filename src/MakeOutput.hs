module MakeOutput
    ( showUnicodeTime,
      playSound,
    ) where

import Data.List (unlines, transpose)
import System.Process (callCommand)

convertTimeToUnicode :: Int -> Int -> [[String]]
convertTimeToUnicode m s =
  (transpose (map convertToUnicode 
      [ unicodeCodes !! (m `div` 10)
      , unicodeCodes !! 11
      , unicodeCodes !! (m `mod` 10)
      , unicodeCodes !! 10
      , unicodeCodes !! (s `div` 10)
      , unicodeCodes !! 11
      , unicodeCodes !! (s `mod` 10)
      ]))
    where
        convertToUnicode :: (Int, Int, Int, Int, Int) -> [String]
        convertToUnicode (i0, i1, i2, i3, i4) = 
          [unicodeRows !! i0,
           unicodeRows !! i1,
           unicodeRows !! i2,
           unicodeRows !! i3,
           unicodeRows !! i4]
   
        unicodeRows :: [String]
        unicodeRows = ["██████", -- 0
                       "████  ", -- 1
                       "  ████", -- 2
                       "██    ", -- 3
                       "  ██  ", -- 4
                       "    ██", -- 5
                       "██  ██", -- 6
                       "      ", -- 7
                       "  "]     -- 8

        unicodeCodes :: [(Int, Int, Int, Int, Int)]
        unicodeCodes = [(0,6,6,6,0),
                      (5,5,5,5,5),
                      (0,5,0,3,0),
                      (0,5,0,5,0),
                      (6,6,0,5,5),
                      (0,3,0,5,0),
                      (0,3,0,6,0),
                      (0,5,5,5,5),
                      (0,6,0,6,0),
                      (0,6,0,5,0),
                      (7,4,7,4,7),
                      (8,8,8,8,8)]

addDescription :: [String] -> Int -> Int -> String -> [String]
addDescription tsList currentCycle maxCycle intervalType =
  let (intervalTypeText, spaces) = case intervalType of
        "work"   -> ("work", "             ")
        "sBreak" -> ("small break", "          ")
        "lBreak" -> ("big break", "           ")
        _        -> (intervalType, "               ")
  in tsList ++ [spaces ++ intervalTypeText ++ ": " ++ show currentCycle ++ 
                " / " ++ show maxCycle]

addPaused :: [String] -> Bool -> [String]
addPaused tsList paused =
  if (paused)
    then do
         ["                paused"] ++ tsList
    else [" "] ++ tsList

addUnicodeBorders :: [[String]] -> [String]
addUnicodeBorders tsList = 
  addUpDownBorders tsList upBorder downBorder leftBorder rightBorder
  where
    upBorder, downBorder, leftBorder, rightBorder :: String
    upBorder = "┌────────────────────────────────────┐"
    downBorder = "└────────────────────────────────────┘"
    leftBorder = "│ "
    rightBorder = " │" 
    addUpDownBorders tsList upBorder downBorder leftBorder rightBorder=
      [upBorder] ++ (map addSideBorders tsList) ++ [downBorder]
    addSideBorders row = leftBorder ++ (concat row) ++ rightBorder

formatPomodoroTime :: Int -> (Int, Int)
formatPomodoroTime t = (t `div` 60, t `mod` 60)

showUnicodeTime :: Int -> Int -> Int -> String -> Bool -> String
showUnicodeTime t currentCycle maxCycle intervalType paused = do 
  let (m, s) = formatPomodoroTime t
  unlines (addPaused 
            (addDescription (addUnicodeBorders (convertTimeToUnicode m s))
              currentCycle maxCycle intervalType)
            paused)

playSound :: Int -> String -> Int -> IO ()
playSound time soundType volume = 
    if time == 0
        then do 
            let resourceFolder = " ./resources/"
                soundFile = case soundType of
                    "work"   -> "pomodoro_work.mp3 "
                    "sBreak" -> "pomodoro_sBreak.mp3 "
                    "lBreak" -> "pomodoro_lBreak.mp3 "
                command = "." ++ resourceFolder ++ "play_with_volume.sh" ++ 
                          resourceFolder ++ soundFile ++ (show volume)
            callCommand command
        else return ()
