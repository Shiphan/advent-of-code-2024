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

textToTwoList :: Text -> ([Int], [Int])
textToTwoList text =
  let array = map lineToTwoInt $ filter (/= "") $ splitOn "\n" text
   in (map (\x -> let (a, _) = x in a) array, map (\x -> let (_, a) = x in a) array)

main = do
  file <- readFile "./day1.txt"
  print $
    sum $
      map (\(a, b) -> abs $ a - b) $
        uncurry zip $
          let (a, b) = textToTwoList $ fromString file in (sort a, sort b)
