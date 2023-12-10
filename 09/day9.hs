module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parseLine :: T.Text -> [Int]
parseLine = map (read . T.unpack) . T.words
parse = map parseLine . T.lines

delta :: [Int] -> [Int]
delta vals =
  let pairs = zip (drop 1 vals) vals
  in map (uncurry (-)) pairs

nextVal :: [Int] -> Int
nextVal list | all (== 0) list = 0
nextVal list = last list + nextVal (delta list)

part1 = sum . map nextVal
part2 = part1 . map reverse

main = do
  input <- TIO.readFile "input"
  let values = parse input
  print $ part1 values
  print $ part2 values
