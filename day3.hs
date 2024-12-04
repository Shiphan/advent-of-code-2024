{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isNumber)
import Data.String (IsString (fromString))
import Data.Text (Text, findIndex, splitOn)
import Data.Text.Read (decimal)

findAll :: Char -> String -> [Int]
findAll ch str = map fst $ filter (\x -> snd x == ch) $ zip [0 ..] str

isMul :: String -> Maybe Int
isMul str =
  case take 4 str of
    "mul(" -> case isNum $ drop 4 str of
      Nothing -> Nothing
      Just (len, value) -> case str `at` (4 + len) of
        Just ',' -> case isNum $ drop (5 + len) str of
          Nothing -> Nothing
          Just (len2, value2) -> case str `at` (5 + len + len2) of
            Just ')' -> Just (value * value2)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
  where
    at list index = if length list <= index then Nothing else Just $ list !! index

isNum :: String -> Maybe (Int, Int)
isNum str =
  case findIndex (not . isNumber) $ fromString str of
    Nothing -> if length str > 3 then Nothing else Just (length str, read str)
    Just notNumberIndex' ->
      if notNumberIndex' == 0 || notNumberIndex' > 3
        then Nothing
        else Just (notNumberIndex', read $ take notNumberIndex' str)

main = do
  file <- readFile "./day3.txt"
  print $ sum $ map (\x -> let Just x' = x in x') $ filter (/= Nothing) $ map (isMul . (`drop` file)) $ findAll 'm' file
