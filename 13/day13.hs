module Main where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (listToMaybe, fromJust, fromMaybe)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex func = toList . Seq.mapWithIndex func . Seq.fromList

type Position = (Int, Int)
data RockMap = RockMap { rocks :: Set.Set Position, width :: Int, height :: Int } deriving (Show)

parseMap :: T.Text -> RockMap
parseMap text =
  let rows@(firstRow : _) = T.lines text
      height = length rows
      width = T.length firstRow
      rocks = Set.fromList $ concat $ mapWithIndex parseMap' $ T.lines text
  in RockMap { rocks, width, height }
  where
    parseMap' y line =
      let coords = mapWithIndex (\x rock -> ((x, y), rock)) (T.unpack line)
      in map fst $ filter (\(_, rock) -> rock == '#') coords

parseMaps :: T.Text -> [RockMap]
parseMaps text = map parseMap (T.splitOn (T.pack "\n\n") text)

extractCol :: Int -> RockMap -> Set.Set Int
extractCol x = Set.map snd . Set.filter (\(x', _) -> x' == x) . rocks
extractRow :: Int -> RockMap -> Set.Set Int
extractRow y = Set.map fst . Set.filter (\(_, y') -> y' == y) . rocks

extractCols :: RockMap -> [Set.Set Int]
extractCols rockMap = [extractCol x rockMap | x <- [0..width rockMap - 1]]
extractRows :: RockMap -> [Set.Set Int]
extractRows rockMap = [extractRow y rockMap | y <- [0..height rockMap - 1]]

symmetricDiffs :: Seq.Seq (Set.Set Int) -> Int -> Int -> Int
symmetricDiffs seq l r
  | l < 0      = 0
  | r >= bound = 0
  | otherwise  = setDiffs (Seq.index seq l) (Seq.index seq r) + symmetricDiffs seq (l - 1) (r + 1)
  where
    bound = Seq.length seq
    setDiffs a b = Set.size (Set.difference a b) + Set.size (Set.difference b a)

symmetricOn :: Int -> [Set.Set Int] -> Maybe Int
symmetricOn diffs groups =
  let seq = Seq.fromList groups
      bounds = [0 .. Seq.length seq - 2]
  in listToMaybe [x | x <- bounds, symmetricDiffs seq x (x + 1) == diffs]

symmetricHorizontally :: Int -> RockMap -> Maybe Int
symmetricHorizontally diffs = symmetricOn diffs . extractCols
symmetricVertically :: Int -> RockMap -> Maybe Int
symmetricVertically diffs = symmetricOn diffs . extractRows

summarize :: Int -> RockMap -> Int
summarize diffs rockMap =
  let hSymmetry = maybe 0 (+1) (symmetricHorizontally diffs rockMap)
      vSymmetry = maybe 0 (+1) (symmetricVertically diffs rockMap)
  in hSymmetry + (100 * vSymmetry)

part1 :: [RockMap] -> Int
part1 maps = sum $ map (summarize 0) maps

part2 :: [RockMap] -> Int
part2 maps = sum $ map (summarize 1) maps

main = do
  input <- TIO.readFile "input"
  let maps = parseMaps input
  print "hello"
  print $ part1 maps
  print $ part2 maps
