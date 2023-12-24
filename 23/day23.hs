module Main where

import qualified Data.Array as A
import Data.List (find, sort, groupBy, nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex mapper = zipWith mapper [0..]

toGroups :: Ord k => Ord v => [(k, v)] -> [(k, [v])]
toGroups = map (\pairs@((key, val):_) -> (key, map snd pairs)) . groupBy (\a b -> fst a == fst b) . sort

type Position = (Int, Int)
neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

type TrailMap = A.Array Position Char
inBounds :: TrailMap -> Position -> Bool
inBounds trail (x, y) =
  let ((loX, loY), (hiX, hiY)) = A.bounds trail
  in loX <= x && x <= hiX && loY <= y && y <= hiY

parseTrail :: T.Text -> TrailMap
parseTrail text =
  let lines@(firstLine:_) = T.lines text
      height = length lines
      width = T.length firstLine
      cells = concat $ mapWithIndex parseLine lines
  in A.array ((0, 0), (width - 1, height - 1)) cells
  where
    parseLine y line = mapWithIndex (`parseCell` y) (T.unpack line)
    parseCell x y char = ((x, y), char)

isPathway :: TrailMap -> Position -> Bool
isPathway trail pos = inBounds trail pos && trail A.! pos /= '#'

canMove :: TrailMap -> Position -> Position -> Bool
canMove trail (fromX, fromY) to@(toX, toY) =
  let delta = (toX - fromX, toY - fromY)
  in inBounds trail to && canMove' (trail A.! to) delta
  where
    canMove' '.' _ = True
    canMove' '^' (0, -1) = True
    canMove' 'v' (0, 1) = True
    canMove' '<' (-1, 0) = True
    canMove' '>' (1, 0) = True
    canMove' _ _ = False

data Graph = Graph {
  connections :: M.Map Position [(Position, Int)],
  start :: Position,
  end :: Position
} deriving (Show)

trailToGraph :: TrailMap -> Graph
trailToGraph trail =
  let connections = findAllConnections trail
      Just start = find (\(_, y) -> y == snd (fst $ A.bounds trail)) $ M.keys connections
      Just end = find (\(_, y) -> y == snd (snd $ A.bounds trail)) $ M.keys connections
  in Graph connections start end

findAllConnections :: TrailMap -> M.Map Position [(Position, Int)]
findAllConnections trail =
  let nodes = S.fromList $ filter isNode $ A.indices trail
  in M.fromList $ map (\n -> (n, findConnections trail nodes n)) (S.toList nodes)
  where
    isNode pos = isPathway trail pos && length (filter (isPathway trail) $ neighbors pos) /= 2

findConnections :: TrailMap -> S.Set Position -> Position -> [(Position, Int)]
findConnections trail nodes pos =
  let outgoing = filter (isPathway trail) (neighbors pos)
  in mapMaybe (findConnection 1 pos) outgoing
  where
    findConnection dist _ curr | S.member curr nodes = Just (curr, dist)
    findConnection dist prev curr =
      case filter (\pos -> pos /= prev && canMove trail curr pos) (neighbors curr) of
        [next] -> findConnection (dist + 1) curr next
        [] -> Nothing

longestPath :: Graph -> Int
longestPath (Graph connections start end) =
  let Just answer = longestPath' S.empty start
  in answer
  where
    longestPath' _ at | at == end = Just 0
    longestPath' seen at =
      let seen' = S.insert at seen
          nexts = filter (\(pos, _) -> S.notMember pos seen) $ connections M.! at
          dists = mapMaybe (\(pos, dist) -> (+ dist) <$> longestPath' seen' pos) nexts
      in if null dists
        then Nothing
        else Just (maximum dists)

part1 :: TrailMap -> Int
part1 trail =
  let graph = trailToGraph trail
  in longestPath graph

fullyConnect :: Graph -> Graph
fullyConnect (Graph connections start end) =
  let forwardConns = concatMap (\(from, tos) -> map (\(to, dist) -> (from, (to, dist))) tos) $ M.toList connections
      backwardConns = map (\(from, (to, dist)) -> (to, (from, dist))) forwardConns
      allConns = nub $ filter (\(from, (to, _)) -> from /= end && to /= start) $ sort (forwardConns ++ backwardConns)
      connMap = M.fromList $ toGroups allConns
  in Graph connMap start end

part2 :: TrailMap -> Int
part2 trail =
  let graph = trailToGraph trail
      graph' = fullyConnect graph
  in longestPath graph'

main = do
  input <- TIO.readFile "input"
  let trail = parseTrail input
  print $ part1 trail
  print $ part2 trail