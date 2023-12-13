module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)

parseRow :: T.Text -> (T.Text, [Int])
parseRow row =
  let [chars, csv] = T.words row
      widths = map (read . T.unpack) $ T.split (== ',') csv
  in (chars, widths)

parseRows :: T.Text -> [(T.Text, [Int])]
parseRows input = map parseRow $ T.lines input

canBeFull :: Char -> Bool
canBeFull c = c /= '.'

canBeEmpty :: Char -> Bool
canBeEmpty c = c /= '#'

canStartWith :: T.Text -> Int -> Bool
canStartWith text count =
  T.length text >= count &&
  T.all canBeFull (T.take count text) &&
  (T.length text == count || canBeEmpty (T.index text count))

minNecessaryWidth :: [Int] -> Int
minNecessaryWidth widths = sum widths + length widths

combos :: T.Text -> [Int] -> Int
combos text widths = snd $ zombos Map.empty text widths

type Memo = Map.Map (T.Text, [Int]) Int

memoize :: Memo -> T.Text -> [Int] -> Int -> (Memo, Int)
memoize memo text widths result = (Map.insert (text, widths) result memo, result)

-- memoiZed cOMBOS
zombos :: Memo -> T.Text -> [Int] -> (Memo, Int)
zombos memo text widths | Map.member (text, widths) memo
  = (memo, fromJust $ Map.lookup (text, widths) memo)
zombos memo text [] = memoize memo text [] (if T.all canBeEmpty text then 1 else 0)
zombos memo text (width : widths) =
  let
    (memo', startingHere) =
      if canStartWith text width
        then zombos memo (T.drop (width + 1) text) widths
        else (memo, 0)
    (memo'', startingLater) =
      if T.length text >= minNecessaryWidth (width : widths) && canBeEmpty (T.head text)
        then zombos memo' (T.drop 1 text) (width : widths)
        else (memo', 0)
  in memoize memo'' text (width : widths) (startingHere + startingLater)
  
part1 :: [(T.Text, [Int])] -> Int
part1 rows =
  let counts = map (uncurry combos) rows
  in sum counts

unfoldRow :: (T.Text, [Int]) -> (T.Text, [Int])
unfoldRow (chars, widths) =
  let newChars = T.intercalate (T.pack "?") (replicate 5 chars)
      newWidths = concat $ replicate 5 widths
  in (newChars, newWidths)

part2 :: [(T.Text, [Int])] -> Int
part2 rows =
  let unfolded = map unfoldRow rows
      counts = map (uncurry combos) unfolded
  in sum counts

main = do
  input <- TIO.readFile "input"
  let rows = parseRows input
  print $ part1 rows
  print $ part2 rows
