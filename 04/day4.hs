module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intersect)
import Data.Char (isDigit)

data Card =  Card { gameNumber :: Int, want :: [Int], have :: [Int]}

parseCard :: T.Text -> Card
parseCard line =
  let
    [gameStr, parts] = T.splitOn (T.pack ": ") line
    [want, have] = T.splitOn (T.pack " | ") parts
  in Card (parseNum gameStr) (parseSsv want) (parseSsv have)
  where
    parseNum text = read $ T.unpack $ T.dropWhile (not . isDigit) text
    parseSsv text = map (read . T.unpack) $ T.words text

cardWins :: Card -> Int
cardWins (Card _ want have) = length (want `intersect` have)

cardScore :: Card -> Int
cardScore card =
  let wins = cardWins card
  in
    if wins > 0
      then 2 ^ (wins - 1)
      else 0

part1 :: [Card] -> Int
part1 cards =
  let scores = map cardScore cards
  in sum scores

countCards :: [Card] -> Map.Map Int Int -> Int
countCards [] counts = sum (Map.elems counts)
countCards (card : rest) counts =
  let
    cardNum = gameNumber card
    wins = cardWins card
    cardCount = counts Map.! cardNum
    additionalCounts = Map.fromList $ map (, cardCount) [(cardNum+1)..(cardNum+wins)]
    counts' = Map.unionWith (+) counts additionalCounts
  in countCards rest counts'

part2 :: [Card] -> Int
part2 cards = 
  let initialCounts = Map.fromList $ map (, 1) [1..(length cards)]
  in countCards cards initialCounts

main = do
  input <- TIO.readFile "input"
  let cards = map parseCard $ T.lines input
  print $ part1 cards
  print $ part2 cards