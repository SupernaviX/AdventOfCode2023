module Main where

import Data.List (foldl', partition)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe func start =
  start : case func start of
    Just next -> iterateMaybe func next
    Nothing -> []

type Wire = (T.Text, T.Text)
parseWiring :: T.Text -> [Wire]
parseWiring = concatMap parseComponent . T.lines
  where
    parseComponent line =
      let [from, tos] = T.splitOn (T.pack ": ") line
      in map (from,) (T.words tos)

type Graph = M.Map T.Text (S.Set T.Text)

wiresToGraph :: [Wire] -> Graph
wiresToGraph = foldl' addWire M.empty

addWire :: Graph -> Wire -> Graph
addWire graph (from, to) = addWire' (addWire' graph from to) to from
  where
    addWire' g from to = M.insertWith S.union from (S.singleton to) g

removeWire :: Graph -> (T.Text, T.Text) -> Graph
removeWire graph (from, to) = removeWire' (removeWire' graph from to) to from
  where
    removeWire' g from to = M.adjust (S.delete to) from g

removeComponent :: Graph -> T.Text -> Graph
removeComponent graph comp = removeComponents graph (S.singleton comp)

removeComponents :: Graph -> S.Set T.Text -> Graph
removeComponents graph comps = M.mapMaybeWithKey removeComps' graph
  where
    removeComps' from tos =
      if S.member from comps
        then Nothing
        else Just (S.difference tos comps)

partitionGraph :: Graph -> ([T.Text], [T.Text])
partitionGraph graph =
  let firstComp:otherKeys = M.keys graph
      (_, p1, p2) = foldl' (choosePartition firstComp) (graph, [firstComp], []) otherKeys
  in (p1, p2)
  where
    choosePartition firstComp (g, p1, p2) comp =
      if inSameCluster g firstComp comp
        then (g, comp:p1, p2)
        else (removeComponent g comp, p1, comp:p2)

inSameCluster :: Graph -> T.Text -> T.Text -> Bool
inSameCluster graph start end | start == end = True
inSameCluster graph start end =
  let removeList = iterateMaybe (removePath start end) graph
  in length removeList > 4

removePath :: T.Text -> T.Text -> Graph -> Maybe Graph
removePath from to graph = do
  path <- findPath from to graph
  if S.null path
    then Just (removeWire graph (from, to))
    else Just (removeComponents graph path)

findPath :: T.Text -> T.Text -> Graph -> Maybe (S.Set T.Text)
findPath from to graph = do
  path <- findPath' S.empty (Seq.singleton [from])
  Just (S.delete from (S.fromList path))
  where
    findPath' _ Seq.Empty = Nothing
    findPath' _ ((next:path) Seq.:<| _) | next == to = Just path
    findPath' seen ((next:path) Seq.:<| paths) =
      let neighbors = S.filter (`S.notMember` seen) $ graph M.! next
          seen' = S.union neighbors seen
          paths' = paths Seq.>< Seq.fromList (map (:next:path) (S.toList neighbors))
      in findPath' seen' paths'

main = do
  input <- TIO.readFile "input"
  let wires = parseWiring input
  let graph = wiresToGraph wires
  let (firstGroup, secondGroup) = partitionGraph graph
  print $ length firstGroup * length secondGroup
