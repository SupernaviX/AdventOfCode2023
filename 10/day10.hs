module Main where

import qualified Data.Map as Map
import Data.Foldable (foldl', toList)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: forall a b. (Int -> a -> b) -> [a] -> [b]
mapWithIndex mapper = toList . Seq.mapWithIndex mapper . Seq.fromList

data Direction = North | South | East | West deriving (Eq, Ord, Enum, Show)
otherDir :: Direction -> Direction
otherDir North = South
otherDir South = North
otherDir East = West
otherDir West = East

type Position = (Int, Int)
move :: Position -> Direction -> Position
move (x, y) North = (x, y - 1)
move (x, y) South = (x, y + 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)
move2 :: Position -> Direction -> Direction -> Position
move2 pos d1 = move (move pos d1)

type Pipe = Set.Set Direction
type PipeMap = Map.Map Position Pipe

data Cell = Start | Full Pipe | Empty
parseCell :: Char -> Cell
parseCell '|' = Full (Set.fromList [North, South])
parseCell '-' = Full (Set.fromList [East, West])
parseCell 'L' = Full (Set.fromList [North, East])
parseCell 'J' = Full (Set.fromList [North, West])
parseCell 'F' = Full (Set.fromList [South, East])
parseCell '7' = Full (Set.fromList [South, West])
parseCell 'S' = Start
parseCell '.' = Empty

parseRow :: Int -> T.Text -> ([(Position, Pipe)], Maybe Position)
parseRow y row =
  let chars = T.unpack row
      indexedChars = mapWithIndex (\i c -> (i, parseCell c)) chars
      startState = ([], Nothing)
  in foldl' parseRow' startState indexedChars
    where parseRow' result         (_, Empty)     = result
          parseRow' (pipes, _)     (x, Start)     = (pipes, Just (x, y))
          parseRow' (pipes, start) (x, Full pipe) = (((x, y), pipe) : pipes, start)

hasConnectionFrom :: PipeMap -> Position -> Direction -> Bool
hasConnectionFrom pipes pos dir =
  let nextPos = move pos dir
      nextPipe = Map.findWithDefault Set.empty nextPos pipes
      connectingDir = otherDir dir
  in Set.member connectingDir nextPipe

neighborDirs :: PipeMap -> Position -> [Direction]
neighborDirs pipes from = filter (hasConnectionFrom pipes from) (enumFrom North)

connectedPipes :: PipeMap -> Position -> [Position]
connectedPipes pipes from =
  let connectedDirs = maybe [] Set.toList (Map.lookup from pipes)
  in map (move from) connectedDirs

parseMap :: T.Text -> (PipeMap, Position)
parseMap input =
  let rows = mapWithIndex parseRow (T.lines input)
      pipes = Map.fromList (concatMap fst rows)
      [startPos] = mapMaybe snd rows
      startPipe = Set.fromList (neighborDirs pipes startPos)
  in (Map.insert startPos startPipe pipes, startPos)

findPipeDistances :: PipeMap -> Map.Map Position Int -> [(Position, Int)] -> Map.Map Position Int
findPipeDistances pipes distances [] = distances
findPipeDistances pipes distances ((next, _) : frontier)
  | Map.member next distances = findPipeDistances pipes distances frontier
findPipeDistances pipes distances ((next, distance) : frontier) =
  let candidates = connectedPipes pipes next
      unvisited = filter (`Map.notMember` distances) candidates
      distances' = Map.insert next distance distances
      frontier' = frontier ++ map (, distance + 1) unvisited
  in findPipeDistances pipes distances' frontier'

part1 :: PipeMap -> Position -> Int
part1 pipes start =
  let distances = findPipeDistances pipes Map.empty [(start, 0)]
  in maximum (Map.elems distances)

zoomIn :: Position -> Position
zoomIn (x, y) = (x * 2, y * 2)

-- Map a pipe from the original grid into a 2x2 space in the new grid.
-- The top-left corner of the space is identical to the original grid,
-- and the right/bottom sides represent spaces between pipes.
zoomInPipeTile :: (Position, Pipe) -> [Position]
zoomInPipeTile (pos, pipe) =
  zoomIn pos :
  [move (zoomIn pos) East | Set.member East pipe] ++
  [move (zoomIn pos) South | Set.member South pipe]

type Bounds = ((Int, Int), (Int, Int))
findBounds :: Set.Set Position -> Bounds
findBounds tiles =
  let xs = Set.map fst tiles
      ys = Set.map snd tiles
  in ((minimum xs, minimum ys), (maximum xs, maximum ys))

data PipeInfo = PipeInfo { fullTiles :: Set.Set Position, bounds :: Bounds } deriving (Show)
isFree :: PipeInfo -> Position -> Bool
isFree info pos = Set.notMember pos (fullTiles info)

isOutOfBounds :: PipeInfo -> Position -> Bool
isOutOfBounds info (x, y) =
  let ((xmin, ymin), (xmax, ymax)) = bounds info
  in x < xmin || y < ymin || x > xmax || y > ymax

processPipeInfo :: PipeMap -> Position -> PipeInfo
processPipeInfo pipes start =
  let distances = findPipeDistances pipes Map.empty [(start, 0)]
      pipeTiles = map (\x -> (x, pipeAt x)) (Map.keys distances)
      zoomedIn = Set.fromList (concatMap zoomInPipeTile pipeTiles)
  in PipeInfo zoomedIn (findBounds zoomedIn)
  where
    pipeAt pos = Map.findWithDefault Set.empty pos pipes

findStartCandidates :: PipeMap -> Position -> [Position]
findStartCandidates pipes pos =
  let
    pipe = Map.findWithDefault Set.empty pos pipes
    pos' = zoomIn pos
  in case Set.elemAt 0 pipe of
    North -> [move2 pos' North East, move2 pos' North West]
    South -> [move2 pos' South East, move2 pos' South West]
    East -> [move2 pos' North East, move2 pos' South East]
    west -> [move2 pos' North West, move2 pos' South West]

tryFillSpace :: PipeInfo -> Position -> Maybe (Set.Set Position)
tryFillSpace info start = tryFillSpace' info (Seq.singleton start) (Set.singleton start)

tryFillSpace' :: PipeInfo -> Seq.Seq Position -> Set.Set Position -> Maybe (Set.Set Position)
tryFillSpace' _ Seq.Empty seen = Just seen
tryFillSpace' info (next Seq.:<| frontier) seen =
  let successors = map (move next) (enumFrom North)
      valid = filter isValid successors
      seen' = Set.union (Set.fromList valid) seen
      frontier' = frontier Seq.>< Seq.fromList valid
  in if any (isOutOfBounds info) valid
        then Nothing
        else tryFillSpace' info frontier' seen'
  where isValid pos = isFree info pos && Set.notMember pos seen

filledVolume :: Set.Set Position -> Int
filledVolume = length . Set.filter (\(x, y) -> even x && even y)

part2 :: PipeMap -> Position -> Int
part2 pipes start =
  let pipeInfo = processPipeInfo pipes start
      candidates = findStartCandidates pipes start
      [filledSpaces] = mapMaybe (tryFillSpace pipeInfo) candidates
  in filledVolume filledSpaces

main = do
  input <- TIO.readFile "input"
  let (pipes, start) = parseMap input
  print $ part1 pipes start
  print $ part2 pipes start