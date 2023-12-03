module Main where

import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (intersect)
import qualified Data.Map as Map
import Data.Sequence (fromList, mapWithIndex)

indexed :: [a] -> [(Int, a)]
indexed = toList . mapWithIndex (,) . fromList

isSym x = not (isDigit x) && ('.' /= x)

findRowSymbols :: String -> [(Int, Char)]
findRowSymbols = filter (isSym . snd) . indexed

findRowNumbers :: String -> [(Int, Int)]
findRowNumbers = findRowNumbers' . indexed
  where
    findRowNumbers' [] = []
    findRowNumbers' row@((x, char) : rest)
      | isDigit char =
        let
          (numChars, rest') = span (isDigit . snd) row
          num = read (map snd numChars)
        in (x, num) : findRowNumbers' rest'
      | otherwise = findRowNumbers' rest

findInRows :: (String -> [(Int, a)]) -> [String] -> [((Int, Int), a)]
findInRows finder input =
  concatMap (\(y, row) ->
    let syms = finder row
    in map (\(x, sym) -> ((x, y), sym)) syms
  ) (indexed input)

findSymbols :: [String] -> [((Int, Int), Char)]
findSymbols = findInRows findRowSymbols

findNumbers :: [String] -> [((Int, Int), Int)]
findNumbers = findInRows findRowNumbers

countDigits :: Int -> Int
countDigits = ceiling . logBase 10.0 . fromIntegral

findNumberNeighbors :: ((Int, Int), Int) -> [(Int, Int)]
findNumberNeighbors ((x, y), num) =
  [(x', y') | x' <- [x - 1, x + numLength], y' <- [y - 1, y, y + 1]]
  ++
  [(x', y') | x' <- [x..(x + numLength - 1)], y' <- [y - 1, y + 1]]
  where numLength = countDigits num

isNextToSymbol :: Map.Map (Int, Int) Char -> ((Int, Int), Int) -> Bool
isNextToSymbol symbols number =
  any (`Map.member` symbols) (findNumberNeighbors number)

findStarNeighbors :: [(Int, Int)] -> ((Int, Int), Int) -> [(Int, Int)]
findStarNeighbors stars number =
  let neighbors = findNumberNeighbors number
  in intersect stars neighbors

part1 :: [String] -> Int
part1 rows =
  let
    symbols = Map.fromList (findSymbols rows)
    numbers = findNumbers rows
    partNumbers = filter (isNextToSymbol symbols) numbers
  in sum (map snd partNumbers)

part2 :: [String] -> Int
part2 rows =
  let
    stars = map fst $ filter (\s -> '*' == snd s) (findSymbols rows)
    numbers = findNumbers rows
    starNeighborPairs = concatMap (\num ->
      let starNeighbors = findStarNeighbors stars num
      in map (, [snd num]) starNeighbors) numbers
    starNeighbors = Map.elems $ Map.fromListWith (++) starNeighborPairs
    gearNeighbors = filter (\s -> 2 == length s) starNeighbors
  in sum (map product gearNeighbors)
main = do
  input <- readFile "input"
  print $ part1 (lines input)
  print $ part2 (lines input)
