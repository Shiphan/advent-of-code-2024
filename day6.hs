{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex, findIndices)
import Data.Maybe (fromMaybe)
import Data.Text (pack, splitOn, unpack)

data Direction = U | R | D | L deriving (Enum, Show)

turnRight :: Direction -> Direction
turnRight d = toEnum $ (\x -> (x + 1) `mod` 4) $ fromEnum d

main = do
  file <- readFile "./day6.txt"
  let (map, (x, y)) = readData file
  let allAfter = findAllAfter map (x, y, U)
  print $ (x, y, U)
  print $ goNext map (x, y, U)
  print $ length $ filter (\(idx, point) -> notElem point $ take idx allAfter) $ zip [0 ..] allAfter

findAllAfter :: [[Bool]] -> (Int, Int, Direction) -> [(Int, Int)]
findAllAfter map (x, y, direction) = case goNext map (x, y, direction) of
  Nothing -> [(x, y)]
  Just next -> (x, y) : findAllAfter map next

goNext :: [[Bool]] -> (Int, Int, Direction) -> Maybe (Int, Int, Direction)
goNext map (x, y, direction) =
  let (x', y') = case direction of
        U -> (x - 1, y)
        D -> (x + 1, y)
        L -> (x, y - 1)
        R -> (x, y + 1)
      xMax = length map
      yMax = length $ head map
   in if x' < 0 || x' >= xMax || y < 0 || y >= yMax
        then Nothing
        else
          if map !! x' !! y'
            then goNext map (x, y, turnRight direction)
            else Just (x', y', direction)

readData :: String -> ([[Bool]], (Int, Int))
readData str =
  let lines = map unpack $ filter (/= "") $ splitOn "\n" $ pack str
      atLine = head $ findIndices (\line -> '^' `elem` line) lines
   in (map (map (== '#')) lines, (atLine, fromMaybe 0 $ elemIndex '^' $ lines !! atLine))
