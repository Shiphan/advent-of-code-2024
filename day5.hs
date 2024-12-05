{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack, splitOn)
import Data.Text.Read (decimal)
import Text.Printf (printf)

type Table = [(Int, [Int])]

main = do
  file <- readFile "./day5.txt"
  let (rules, updates) = split $ pack file
  let rules' = toTable rules
  print $ sum $ map (\list -> list !! (length list `div` 2)) $ filter (isRightOrder rules') updates
  print $ length rules
  print $ sum $ map (length . snd) rules'
  print $ length rules'
  -- print rules'
  print $ length updates
  print $ length $ filter (isRightOrder rules') updates

isRightOrder :: Table -> [Int] -> Bool
isRightOrder table list =
  all
    ( \(i, x) -> case getValueFromTable table x of
        Just ints -> all (`notElem` ints) $ take i list
        Nothing -> True
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
