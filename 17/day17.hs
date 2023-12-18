module Main where

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (nub, group)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex mapper = toList . Seq.mapWithIndex mapper . Seq.fromList

data PQueue a = PQEmpty | PQNode a [PQueue a]

pqEmpty :: Ord a => PQueue a
pqEmpty = PQEmpty

pqSingleton :: Ord a => a -> PQueue a
pqSingleton x = PQNode x []

pqMerge :: Ord a => PQueue a -> PQueue a -> PQueue a
pqMerge pq1@(PQNode x1 ts1) pq2@(PQNode x2 ts2)
  | x1 < x2 = PQNode x1 (pq2:ts1)
  | otherwise = PQNode x2 (pq1:ts2)
pqMerge PQEmpty pq = pq
pqMerge pq PQEmpty = pq

pqPush :: Ord a => a -> PQueue a -> PQueue a
pqPush x pq = pqMerge pq (pqSingleton x)

pqPushAll :: Ord a => PQueue a -> [a] -> PQueue a
pqPushAll = foldr pqPush

pqPop :: Ord a => PQueue a -> (a, PQueue a)
pqPop (PQNode x ts) = (x, mergeChildren ts)
  where
    mergeChildren (pq1:pq2:ts) = pqMerge (pqMerge pq1 pq2) (mergeChildren ts)
    mergeChildren [t] = t
    mergeChildren [] = PQEmpty

type Position = (Int, Int)
data HeatMap = HeatMap {
  tiles :: M.Map Position Int,
  width :: Int,
  height :: Int
} deriving (Show)

heatLossAt :: HeatMap -> Position -> Int
heatLossAt heats pos = fromJust $ M.lookup pos (tiles heats)

endPos :: HeatMap -> Position
endPos heats = (width heats - 1, height heats - 1)

outOfBounds :: HeatMap -> Position -> Bool
outOfBounds heats (x, y) = x < 0 || y < 0 || x >= width heats || y >= height heats

parseHeats :: T.Text -> HeatMap
parseHeats text =
  let lines@(firstLine:_) = T.lines text
      height = length lines
      width = T.length firstLine
      tiles = M.fromList $ concat $ mapWithIndex parseLine lines
  in HeatMap tiles width height
  where
    parseLine y line = mapWithIndex (`parseCell` y) (T.unpack line)
    parseCell x y cell = ((x, y), digitToInt cell)

type MoveValidator = [Position] -> Bool
type MoveValidators = (MoveValidator, MoveValidator)

hottestPath :: HeatMap -> MoveValidators -> [Position]
hottestPath heats validators =
  let pq = pqSingleton (0, [(0, 0)])
  in hottestPath' heats validators S.empty pq

hottestPath' :: HeatMap -> MoveValidators -> S.Set Approach -> PQueue (Int, [Position]) -> [Position]
hottestPath' heats validators@(legalNextMove, legalEndState) seen pq =
  let (queueItem, pq') = pqPop pq
      (heatLoss, path@(lastPos:_)) = queueItem
      nextPaths = map (:path) (neighbors lastPos)
      validPaths = filter validPath nextPaths
      seen' = S.union (S.fromList (map pathToApproach validPaths)) seen
      pq'' = pqPushAll pq' $ map toQueueItem validPaths
      toQueueItem path@(lastPos:_) =
        let totalHeatLoss = heatLoss + heatLossAt heats lastPos
        in (totalHeatLoss, path)
  in if lastPos == endPos heats && legalEndState path
    then path
    else hottestPath' heats validators seen' pq''
  where
    validPath path = legalNextMove path && S.notMember (pathToApproach path) seen

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

type Delta = (Int, Int)
delta :: Position -> Position -> Delta
delta (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

type Approach = (Position, Maybe [Delta])
pathToApproach :: [Position] -> Approach
pathToApproach path@(lastPos:_) =
  let deltas = take 11 $ zipWith delta (drop 1 path) path
      straightLineTime = listToMaybe (group deltas)
  in (lastPos, straightLineTime)

totalHeatLoss :: HeatMap -> [Position] -> Int
totalHeatLoss heats path =
  let path' = drop 1 $ reverse path
  in sum $ map (heatLossAt heats) path'

legalNextMoveP1 :: HeatMap -> MoveValidator
legalNextMoveP1 heats (move:_) | outOfBounds heats move    = False
legalNextMoveP1 heats (move:_:oldMove:_) | move == oldMove = False
legalNextMoveP1 heats moves | length moves < 5             = True
legalNextMoveP1 heats moves =
  let deltas = take 4 $ zipWith delta (drop 1 moves) moves
  in length (nub deltas) > 1

part1 :: HeatMap -> Int
part1 heats =
  let validators = (legalNextMoveP1 heats, const True)
      path = hottestPath heats validators
  in totalHeatLoss heats path

legalNextMoveP2 :: HeatMap -> MoveValidator
legalNextMoveP2 heats (move:_) | outOfBounds heats move    = False
legalNextMoveP2 heats (move:_:oldMove:_) | move == oldMove = False
legalNextMoveP2 heats moves =
  let deltas = take 11 $ zipWith delta (drop 1 moves) moves
  in case map length (group deltas) of
    [] -> True
    (1:prevStreak:_)  -> prevStreak >= 4
    (currStreak:_)    -> currStreak <= 10

legalEndStateP2 :: MoveValidator
legalEndStateP2 moves =
  let deltas = take 11 $ zipWith delta (drop 1 moves) moves
      (x:_) = map length (group deltas)
  in x >= 4 && x <= 10


part2 :: HeatMap -> Int
part2 heats =
  let validators = (legalNextMoveP2 heats, legalEndStateP2)
      path = hottestPath heats validators
  in totalHeatLoss heats path

main = do
  input <- TIO.readFile "input"
  let heats = parseHeats input
  print $ part1 heats
  print $ part2 heats
