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
type Mirror = Char
data MirrorMap = MirrorMap {
  mirrors :: M.Map Position Mirror,
  width :: Int,
  height :: Int
} deriving (Show)

mirrorAt :: MirrorMap -> Position -> Maybe Mirror
mirrorAt mirrorMap pos = M.lookup pos (mirrors mirrorMap)

outOfBounds :: MirrorMap -> Position -> Bool
outOfBounds mirrorMap (x, y) = x < 0 || y < 0 || x >= width mirrorMap || y >= height mirrorMap

parseMap :: T.Text -> MirrorMap
parseMap text =
  let rows@(firstRow:_) = T.lines text
      height = length rows
      width = T.length firstRow
      mirrors = M.fromList $ concat $ mapWithIndex parseRow (T.lines text)
      parseRow y row = catMaybes $ mapWithIndex (`parseCell` y) (T.unpack row)
      parseCell x y '.' = Nothing
      parseCell x y chr = Just ((x, y), chr)
  in MirrorMap mirrors width height

data Direction = North | South | East | West deriving (Ord, Eq, Show)

nextPos :: Direction -> Position -> Position
nextPos North (x, y) = (x, y - 1)
nextPos South (x, y) = (x, y + 1)
nextPos West (x, y) = (x - 1, y)
nextPos East (x, y) = (x + 1, y)

isVertical :: Direction -> Bool
isVertical North = True
isVertical South = True
isVertical _ = False

isHorizontal :: Direction -> Bool
isHorizontal = not . isVertical

reflect :: Mirror -> Direction -> [Direction]
reflect '/' North = [East]
reflect '/' South = [West]
reflect '/' East = [North]
reflect '/' West = [South]
reflect '\\' North = [West]
reflect '\\' South = [East]
reflect '\\' East = [South]
reflect '\\' West = [North]
reflect '|' dir = if isVertical dir then [dir] else [North, South]
reflect '-' dir = if isHorizontal dir then [dir] else [East, West]

type Vector = (Position, Direction)

beamFromVector :: MirrorMap -> Vector -> ([Position], [Vector])
beamFromVector mirrors (pos, dir) =
  let pos' = nextPos dir pos
      (path, succs) = beamFromVector mirrors (pos', dir)
  in if outOfBounds mirrors pos'
    then ([], [])
    else case mirrorAt mirrors pos' of
      Just mirror -> ([pos'], map (pos',) $ reflect mirror dir)
      Nothing -> (pos' : path, succs)

startCandidates :: MirrorMap -> [Vector]
startCandidates mirrorMap = concat $
  [[((-1, y), East), ((width mirrorMap, y), West)] | y <- [0..height mirrorMap - 1]] ++
  [[((x, -1), South), ((x, height mirrorMap), North)] | x <- [0..width mirrorMap - 1]]

type BeamMap = M.Map Vector ([Position], [Vector])
beamMap :: MirrorMap -> M.Map Vector ([Position], [Vector])
beamMap mirrorMap =
  let mirrorLocs = M.keys (mirrors mirrorMap)
      mirrorVecs = [(pos, dir) | pos <- mirrorLocs, dir <- [North, South, East, West]]
      interestingVecs = mirrorVecs ++ startCandidates mirrorMap
  in M.fromList $ map (\vec -> (vec, beamFromVector mirrorMap vec)) interestingVecs

hitTiles :: BeamMap -> Vector -> Int
hitTiles beams start =
  let vecs = hitTiles' beams (S.singleton start) [start]
      tiles = concatMap (fst . (beams M.!)) (S.toList vecs)
  in S.size (S.fromList tiles)

hitTiles' :: BeamMap -> S.Set Vector -> [Vector] -> S.Set Vector
hitTiles' _ seen [] = seen
hitTiles' beams seen (vec:vecs) =
  let successors = snd (beams M.! vec)
      valid = filter (`S.notMember` seen) successors
      seen' = S.union (S.fromList valid) seen
      vecs' = vecs ++ valid
  in hitTiles' beams seen' vecs'

part1 :: MirrorMap -> Int
part1 mirrors =
  let beams = beamMap mirrors
  in hitTiles beams ((-1, 0), East)

part2 :: MirrorMap -> Int
part2 mirrors =
  let beams = beamMap mirrors
      starts = startCandidates mirrors
  in maximum (map (hitTiles beams) starts)

main = do
  input <- TIO.readFile "input"
  let mirrors = parseMap input
  print $ part1 mirrors
  print $ part2 mirrors
