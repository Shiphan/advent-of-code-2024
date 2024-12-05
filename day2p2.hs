{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString (fromString))
import Data.Text (Text, splitOn)
import Data.Text.Read (decimal)

main = do
  file <- readFile "./day2.txt"
  print $
    length $
      filter (\ints -> any (handleInts . dropOne ints) [0 .. length ints - 1]) $
        map lineToInts $
          fileToLines file

dropOne :: [Int] -> Int -> [Int]
dropOne [] _ = []
dropOne (x : t) 0 = t
dropOne (x : t) i = x : dropOne t (i - 1)

fileToLines :: String -> [Text]
fileToLines str = filter (/= "") $ splitOn "\n" $ fromString str

lineToInts :: Text -> [Int]
lineToInts text = map toInt $ splitOn " " text
  where
    toInt text = let Right (x, _) = decimal text in x

handleInts :: [Int] -> Bool
handleInts ints = handleInts' increase ints || handleInts' decrease ints

increase :: Int -> Int -> Bool
increase x y = y > x && y - x < 4

decrease :: Int -> Int -> Bool
decrease x y = x > y && x - y < 4

handleInts' :: (Int -> Int -> Bool) -> [Int] -> Bool
handleInts' func (x : y : t) = func x y && handleInts' func (y : t)
handleInts' _ [_] = True
