module Main where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: forall a b. (Int -> a -> b) -> [a] -> [b]
mapWithIndex mapper = toList . Seq.mapWithIndex mapper . Seq.fromList

pairs :: forall a. [a] -> [(a, a)]
pairs [] = []
pairs (x: xs) = map (x,) xs ++ pairs xs

parseRow :: Int -> T.Text -> [Position]
parseRow y line =
  let cells = mapWithIndex parseCell (T.unpack line)
  in catMaybes cells
  where
    parseCell x '#' = Just (x, y)
    parseCell _ '.' = Nothing

type Position = (Int, Int)
type GalaxySet = Set.Set Position
parseGalaxies :: T.Text -> GalaxySet
parseGalaxies input =
  let rows = mapWithIndex parseRow (T.lines input)
  in Set.fromList (concat rows)

expandDimension :: Int -> Set.Set Int -> Seq.Seq Int
expandDimension amount coords =
  let sourceValues = [minimum coords .. maximum coords]
      (_ : expandedValues) = scanl maybeExpand (-1, 0) sourceValues
  in Seq.fromList (map fst expandedValues)
  where
    maybeExpand (_, expansion) val
      | Set.member val coords = (val + expansion, expansion)
      | otherwise = (val + expansion, expansion + (amount - 1))

expandGalaxies :: Int -> GalaxySet -> GalaxySet
expandGalaxies amount init =
  let extantXs = Set.map fst init
      extantYs = Set.map snd init
      newXs = expandDimension amount extantXs
      newYs = expandDimension amount extantYs
  in Set.map (bimap (Seq.index newXs) (Seq.index newYs)) init

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

distanceAfterExpansion :: Int -> GalaxySet -> Int
distanceAfterExpansion amount galaxies =
  let galaxies' = expandGalaxies amount galaxies
      distances = map (uncurry distance) (pairs (Set.toList galaxies'))
  in sum distances

part1 :: GalaxySet -> Int
part1 = distanceAfterExpansion 2

part2 :: GalaxySet -> Int
part2 = distanceAfterExpansion 1000000

main = do
  input <- TIO.readFile "input"
  let galaxies = parseGalaxies input
  print $ part1 galaxies
  print $ part2 galaxies