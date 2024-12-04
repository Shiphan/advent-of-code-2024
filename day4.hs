{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString (fromString))
import Data.Text (Text, index, length, splitOn, unpack)

main = do
  file <- readFile "./day4.txt"
  let lines = fileToLines file
      allPosition = [(x, y) | x <- [0 .. Prelude.length lines - 1], y <- [0 .. Data.Text.length (lines !! x)]]
   in print $ sum $ map (`search` lines) allPosition

fileToLines :: String -> [Text]
fileToLines str = filter (/= "") $ splitOn "\n" $ fromString str

search :: (Int, Int) -> [Text] -> Int
search (x, y) strings =
  let target = "XMAS"
      direction = filter (/= (0, 0)) [(xOff, yOff) | xOff <- [-1 .. 1], yOff <- [-1 .. 1]]
   in Prelude.length $ filter (\off -> search' target 0 off (x, y) strings) direction
  where
    search' targets idx (xOff, yOff) (x, y) strings =
      idx >= Data.Text.length targets
        || case tryIndex targets idx of
          Just targetCh -> case strings `at` x of
            Just text -> case tryIndex text y of
              Just ch -> (ch == targetCh) && search' targets (idx + 1) (xOff, yOff) (x + xOff, y + yOff) strings
              _ -> False
            _ -> False
          _ -> False

at :: [a] -> Int -> Maybe a
at list index =
  if index < 0 || index >= Prelude.length list
    then Nothing
    else Just $ list !! index

tryIndex :: Text -> Int -> Maybe Char
tryIndex text idx =
  if idx < 0 || idx >= Data.Text.length text
    then Nothing
    else Just (index text idx)
