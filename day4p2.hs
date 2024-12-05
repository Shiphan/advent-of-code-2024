{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import Data.Text (Text, index, length, splitOn, unpack)

main = do
  file <- readFile "./day4.txt"
  let lines = fileToLines file
  let allPosition = [(x, y) | x <- [0 .. Prelude.length lines - 1], y <- [0 .. Data.Text.length (lines !! x) - 1]]
  print $ reverse $ filter (`search` lines) allPosition
  print $ Prelude.length $ filter (`search` lines) allPosition

fileToLines :: String -> [Text]
fileToLines str = filter (/= "") $ splitOn "\n" $ fromString str

search :: (Int, Int) -> [Text] -> Bool
search (x, y) strings =
  (strings !! x `index` y == 'A')
    && maybe False matchMSs (take' (x, y) strings)

take' :: (Int, Int) -> [Text] -> Maybe [Char]
take' (x, y) lines =
  let offsets = [(-1, -1), (-1, 1), (1, 1), (1, -1)]
      found = map (\(xOff, yOff) -> tryFind (x + xOff, y + yOff) lines) offsets
   in if all isJust found -- [(Int, Int)] -> [Maybe Char]
        then Just $ map (\x -> let Just x' = x in x') found
        else Nothing
  where
    tryFind (x, y) lines = case lines `at` x of
      Nothing -> Nothing
      Just line -> line `tryIndex` y

matchMSs :: [Char] -> Bool
matchMSs chars = chars `elem` ["MSSM", "MMSS", "SMMS", "SSMM"]

at :: [a] -> Int -> Maybe a
at list index =
  if index < 0 || index >= Prelude.length list
    then Nothing
    else Just $ list !! index

tryIndex :: Text -> Int -> Maybe Char
tryIndex text idx =
  if idx < 0 || idx >= Data.Text.length text
    then Nothing
    else Just $ index text idx
