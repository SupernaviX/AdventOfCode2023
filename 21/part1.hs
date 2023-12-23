module Main where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex mapper = toList . Seq.mapWithIndex mapper . Seq.fromList

type Position = (Int, Int)

data RockMap = RockMap {
  tiles :: S.Set Position,
  width :: Int,
  height :: Int
} deriving (Show)

parseMap :: T.Text -> (RockMap, Position)
parseMap text =
  let lines@(firstLine:_) = T.lines text
      height = length lines
      width = T.length firstLine
      rocks = S.fromList $ concat $ mapWithIndex parseRow lines
  in (RockMap rocks width height, (div height 2, div width 2))
  where
    parseRow y row = catMaybes $ mapWithIndex (`parseCell` y) (T.unpack row)
    parseCell x y '#' = Just (x, y)
    parseCell _ _  _  = Nothing

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

takeStep :: RockMap -> S.Set Position -> S.Set Position
takeStep rocks plots =
  let plots' = S.fromList $ concatMap (filter inBounds . neighbors) (S.toList plots)
  in S.difference plots' (tiles rocks)
  where
    inBounds (x, y) = x >= 0 && y >= 0 && x < width rocks && y < height rocks

part1 :: RockMap -> Position -> Int
part1 rocks start =
  let positions = iterate (takeStep rocks) (S.singleton start)
  in S.size $ positions !! 64

main = do
  input <- TIO.readFile "input"
  let (rocks, start) = parseMap input
  print $ part1 rocks start