module Main where

import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parseSsv :: T.Text -> [Int]
parseSsv line =  map (read . T.unpack) (T.words line)

parseRacesP1 :: T.Text -> [(Int, Int)]
parseRacesP1 input =
  let [times, distances] = map (parseSsv . T.dropWhile (not . isDigit)) (T.lines input)
  in zip times distances

parseRaceP2 :: T.Text -> (Int, Int)
parseRaceP2 input =
  let [time, distance] = map (read . T.unpack . T.filter (/= ' ') . T.dropWhile (not . isDigit)) (T.lines input)
  in (time, distance)

findMinHoldingTime :: (Int, Int) -> Int
findMinHoldingTime (time, distance) = findMinHoldingTime' time distance 1
findMinHoldingTime' time distance holding
  | ((time - holding) * holding) > distance = holding
  | otherwise = findMinHoldingTime' time distance (holding + 1)

solutionCount :: (Int, Int) -> Int
solutionCount (time, distance) =
  let minHoldingTime = findMinHoldingTime (time, distance)
      maxHoldingTime = time - minHoldingTime
  in maxHoldingTime - minHoldingTime + 1

part1 :: T.Text -> Int
part1 input =
  let races = parseRacesP1 input
  in product (map solutionCount races)

part2 :: T.Text -> Int
part2 input =
  let race = parseRaceP2 input
  in solutionCount race

main = do
  input <- TIO.readFile "input"
  print $ part1 input
  print $ part2 input