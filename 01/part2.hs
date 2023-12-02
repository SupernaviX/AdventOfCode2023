module Part2 where

import Data.Char
import Data.List
import qualified Data.List.NonEmpty as NE

findNums :: String -> NE.NonEmpty Int
findNums = NE.fromList . findNums'

findNums' :: String -> [Int]
findNums' "" = []
findNums' str@(first : rest)
  | isPrefixOf "one" str = [1] ++ findNums' rest
  | isPrefixOf "two" str = [2] ++ findNums' rest
  | isPrefixOf "three" str = [3] ++ findNums' rest
  | isPrefixOf "four" str = [4] ++ findNums' rest
  | isPrefixOf "five" str = [5] ++ findNums' rest
  | isPrefixOf "six" str = [6] ++ findNums' rest
  | isPrefixOf "seven" str = [7] ++ findNums' rest
  | isPrefixOf "eight" str = [8] ++ findNums' rest
  | isPrefixOf "nine" str = [9] ++ findNums' rest
  | isDigit first = [digitToInt first] ++ findNums' rest
  | otherwise = findNums' rest

solve :: String -> Int
solve a = (NE.head nums) * 10 + (NE.last nums)
  where nums = findNums a

main = do
  input <- readFile "input"
  let solutions = map solve $ lines input
  let sum = foldl (+) 0 solutions
  print sum