{-# LANGUAGE OverloadedStrings #-}

import Data.List (findIndices)
import Data.Text (Text, pack, splitOn)
import Data.Text.Read (decimal)
import Text.Printf (printf)

type Table = [(Int, [Int])]

main = do
  file <- readFile "./day5.txt"
  let (rules, updates) = split $ pack file
  let rules' = toTable rules
  -- print $ sum $ map getMid $ filter (null . breakingPoint rules') updates
  -- print $ length rules
  -- print $ sum $ map (length . snd) rules'
  -- print $ length rules'
  -- print rules'
  -- print $ length updates
  -- print $ length $ filter (null . breakingPoint rules') updates
  print $ sum $ map (getMid . toCorrectOrder rules') $ filter (not . null . breakingPoint rules') updates

getMid :: [Int] -> Int
getMid list = list !! (length list `div` 2)

toCorrectOrder :: Table -> [Int] -> [Int]
toCorrectOrder table list =
  case breakingPoint table list of
    [] -> list
    points -> toCorrectOrder table $ moveLeft list $ head points
  where
    moveLeft list 0 = list
    moveLeft (x : y : t) 1 = y : x : t
    moveLeft (x : t) idx = x : moveLeft t (idx - 1)

breakingPoint :: Table -> [Int] -> [Int]
breakingPoint table list =
  findIndices
    ( \(i, x) -> case getValueFromTable table x of
        Just ints -> any (`elem` ints) $ take i list
        Nothing -> False
    )
    $ zip [0 ..] list

toTable :: [(Int, Int)] -> Table
toTable source =
  map (\(_, (k, _)) -> (k, map snd $ filter (\x -> fst x == k) source)) $
    filter (\(idx, (k, _)) -> notElem k $ map fst $ take idx source) $
      zip [0 ..] source

getValueFromTable :: Table -> Int -> Maybe [Int]
getValueFromTable [] key = Nothing
getValueFromTable ((k, v) : tail) key
  | key == k = Just v
  | otherwise = getValueFromTable tail key

split :: Text -> ([(Int, Int)], [[Int]])
split str =
  let [rule, update] = splitOn "\n\n" str
      rule' =
        map
          ( \line ->
              let [x, y] = splitOn "|" line
               in let (Right (x', _), Right (y', _)) = (decimal x, decimal y)
                   in (x', y')
          )
          $ filter (/= "")
          $ splitOn "\n" rule
      update' =
        map (map (\x -> let Right (x', _) = decimal x in x') . filter (/= "") . splitOn ",") $
          filter (/= "") $
            splitOn "\n" update
   in (rule', update')
