module Main where

import Data.List (elemIndex, isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)

data Dir = Lft | Rgt deriving (Enum, Eq, Show)
data Node = Node String String deriving (Show)

parse :: T.Text -> ([Dir], Map.Map String Node)
parse input =
  let (directions : _ : nodes) = map T.unpack $ T.lines input
  in (parseDirections directions, parseNodes nodes)
  where
    parseDirections = cycle . map (toEnum . fromJust . (`elemIndex` "LR"))
    parseNodes = Map.fromList . map parseNode
    parseNode node =
      let name = take 3 node
          left = take 3 $ drop 7 node
          right = take 3 $ drop 12 node
      in (name, Node left right)

step :: Map.Map String Node -> Dir -> String -> String
step nodes dir from =
  let Node left right = fromJust (Map.lookup from nodes)
  in case dir of
    Lft -> left
    Rgt -> right

followDirections :: Map.Map String Node -> [Dir] -> String -> [String]
followDirections _ _ "ZZZ" = []
followDirections nodes (dir:nextDirs) from =
  let next = step nodes dir from
  in next : followDirections nodes nextDirs next

part1 :: Map.Map String Node -> [Dir] -> Int
part1 nodes directions = length (followDirections nodes directions "AAA")

followDirectionsToAny :: Map.Map String Node -> [Dir] -> String -> [String]
followDirectionsToAny nodes (dir:nextDirs) from
  | "Z" `isSuffixOf` from = []
  | otherwise =
      let next = step nodes dir from
      in next : followDirectionsToAny nodes nextDirs next

part2 :: Map.Map String Node -> [Dir] -> Int
part2 nodes directions =
  let starts = filter (isSuffixOf "A") (Map.keys nodes)
      offsets = map (length . followDirectionsToAny nodes directions) starts
  in foldr lcm 1 offsets

main = do
  input <- TIO.readFile "input"
  let (directions, nodes) = parse input
  print $ part1 nodes directions
  print $ part2 nodes directions
