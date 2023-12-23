module Main where

import Data.Foldable (toList)
import qualified Data.Map as M
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
  side :: Int,
  apothem :: Int
} deriving (Show)

parseMap :: T.Text -> RockMap
parseMap text =
  let lines = T.lines text
      side = length lines
      apothem = div side 2
      tiles = S.fromList $ concat $ mapWithIndex parseRow lines
  in RockMap tiles side apothem
  where
    parseRow y row = catMaybes $ mapWithIndex (`parseCell` y) (T.unpack row)
    parseCell x y '#' = Just (x, y)
    parseCell _ _  _  = Nothing

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

type FilledGrid = M.Map Position Int

countFilled :: RockMap -> Int -> Int
countFilled rocks steps =
  let center = (apothem rocks, apothem rocks)
      filled = fillMap (M.singleton center 0) [(center, 0)]
      cond = if even steps then even else odd
  in length $ filter cond $ M.elems filled
  where
    fillMap seen [] = seen
    fillMap seen ((pos, distance):plots) =
      let successors = map (,distance + 1) $ filter isValid $ neighbors pos
          seen' = M.union (M.fromList successors) seen
          plots' = plots ++ successors
      in fillMap seen' plots'
      where
        isValid pos = distance < steps && S.notMember (wrap pos) (tiles rocks) && M.notMember pos seen
        wrap (x, y) = (mod x (side rocks), mod y (side rocks))

lagrange :: [(Integer, Integer)] -> Integer -> Integer
lagrange [(x1, y1), (x2, y2), (x3, y3)] x =
  let t1 = div ((x - x2) * (x - x3) * y1) ((x1 - x2) * (x1 - x3))
      t2 = div ((x - x1) * (x - x3) * y2) ((x2 - x1) * (x2 - x3))
      t3 = div ((x - x1) * (x - x2) * y3) ((x3 - x1) * (x3 - x2))
  in t1 + t2 + t3

part2 :: RockMap -> Integer -> Integer
part2 rocks x =
  let [x1, x2, x3] = [apothem rocks + (x * side rocks) | x <- [0 .. 2]]
      [y1, y2, y3] = map (countFilled rocks) [x1, x2, x3]
      points = [(x1, y1), (x2, y2), (x3, y3)]
  in lagrange (map highPres points) x
  where
    highPres (x, y) = (toInteger x, toInteger y)


main = do
  input <- TIO.readFile "input"
  let rocks = parseMap input
  print $ part2 rocks 26501365