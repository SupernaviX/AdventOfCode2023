module Part1 where

import Data.Char
import Data.List
import Data.Maybe

helper :: String -> Int
helper s = fromJust $ fmap digitToInt $ find isDigit s

solve :: String -> Int
solve str =
  let
    first = firstDigit str
    last = firstDigit (reverse str)
  in (first * 10 + last)
  where
      firstDigit s = fromJust $ fmap digitToInt $ find isDigit s

main :: IO ()
main = do
  input <- readFile "input"
  let solutions = map solve $ lines input
  let sum = foldl (+) 0 solutions
  print sum