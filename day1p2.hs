{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.String (IsString (fromString))
import Data.Text (Text, empty, splitOn)
import Data.Text.Read (decimal)

lineToTwoInt :: Text -> (Int, Int)
lineToTwoInt text =
  let [x, y] = splitOn "   " text
   in let (Right (z, _), Right (w, _)) = (decimal x, decimal y)
       in (z, w)

textToTwoList :: [Text] -> ([Int], [Int])
textToTwoList texts = unzip $ map lineToTwoInt texts

fileToLines :: String -> [Text]
fileToLines str = filter (/= "") $ splitOn "\n" $ fromString str

main = do
  file <- readFile "./day1.txt"
  let (left, right) = textToTwoList $ fileToLines file
   in print $ sum $ map (\a -> a * length (filter (== a) right)) left
