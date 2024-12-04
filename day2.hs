{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString (fromString))
import Data.Text (Text, splitOn)
import Data.Text.Read (decimal)

lineToInts :: Text -> [Int]
lineToInts text = map (\x -> let Right (x', _) = decimal x in x') $ splitOn " " text

handleInts :: [Int] -> Bool
handleInts all@(x : y : _)
  | x > y = handle (\z w -> z > w && z - w < 4) all
  | x < y = handle (\z w -> z < w && w - z < 4) all
  | otherwise = False
  where
    handle func [x, y] = func x y
    handle func (x : y : t) = func x y && handle func (y : t)

main = do
  file <- readFile "./day2.txt"
  print $ length $ filter (handleInts . lineToInts) $ filter (/= "") $ splitOn "\n" $ fromString file
