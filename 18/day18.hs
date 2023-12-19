module Main where

import Control.Exception (assert)
import Data.List (foldl', nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Direction = North | South | East | West deriving (Eq, Ord, Show)

type Position = (Int, Int)
move :: Direction -> Position -> Int -> Position
move North (x, y) dist = (x, y - dist)
move South (x, y) dist = (x, y + dist)
move West  (x, y) dist = (x - dist, y)
move East  (x, y) dist = (x + dist, y)

data Step = Step Direction Int T.Text deriving (Show)

parseSteps :: T.Text -> [Step]
parseSteps text = map parseStep $ T.lines text
  where
    parseStep line =
      let [rawDir, rawDist, rawColor] = T.words line
          dir = parseDir (T.index rawDir 0)
          parseDir 'U' = North
          parseDir 'D' = South
          parseDir 'L' = West
          parseDir 'R' = East
          dist = read $ T.unpack rawDist
          color = T.drop 2 $ T.dropEnd 1 rawColor
      in Step dir dist color

filledArea :: [Step] -> Int
filledArea steps =
  let lines = findVerticalLines steps
  in areaBetweenVerticalLines lines

data VerticalLine = VerticalLine {
  yStart :: Int,
  yEnd :: Int,
  x :: Int
} deriving (Ord, Eq, Show)

findVerticalLines :: [Step] -> [VerticalLine]
findVerticalLines steps =
  let start = (0, 0)
      (end, lines) = foldl' followStep (start, []) steps
  in assert (start == end) sort lines
  where
    followStep (pos@(x, y1), lines) (Step dir dist _) =
      let pos'@(_, y2) = move dir pos dist
          newLines North = [VerticalLine y2 y1 x]
          newLines South = [VerticalLine y1 y2 x]
          newLines _ = []
          lines' = newLines dir ++ lines
      in (pos', lines')

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x1:x2:xs) = (x1, x2) : pairs xs

areaBetweenVerticalLines :: [VerticalLine] -> Int
areaBetweenVerticalLines lines =
  let ends = nub $ sort $ concatMap (\(VerticalLine y1 y2 _) -> [y1, y2]) lines
      regions = zipWith (regionArea lines) ends (drop 1 ends)
  in sum regions

regionArea :: [VerticalLine] -> Int -> Int -> Int
regionArea lines y1 y2 =
  let currentLines = filter partOfThisRegion lines
      currentXRanges = pairs $ nub $ sort $ map x currentLines
      currentXArea = sum $ map rangeSize currentXRanges
      currentYArea = rangeSize (y1, y2)
      prevLines = filter partOfLastRegion lines
      prevXRanges = pairs $ nub $ sort $ map x prevLines
      overlappingXRanges = overlappingRegions currentXRanges prevXRanges
      overlappingXArea = sum $ map rangeSize overlappingXRanges
  in currentXArea * currentYArea - overlappingXArea
  where
    partOfThisRegion (VerticalLine yStart yEnd _) = yEnd > y1 && yStart < y2
    partOfLastRegion (VerticalLine yStart yEnd _) = yEnd >= y1 && yStart < y1
    rangeSize (x1, x2) = x2 - x1 + 1

overlappingRegions :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
overlappingRegions t1s [] = []
overlappingRegions [] t2s = []
overlappingRegions t1s@((t1Start, t1End):t1s') t2s@((t2Start, t2End):t2s')
  | t1End < t2Start = overlappingRegions t1s' t2s
  | t2End < t1Start = overlappingRegions t1s t2s'
  | otherwise =
    let overlapStart = max t1Start t2Start
        overlapEnd = min t1End t2End
        t1Left = [(overlapEnd + 1, t1End) | overlapEnd /= t1End]
        t2Left = [(overlapEnd + 1, t2End) | overlapEnd /= t2End]
    in (overlapStart, overlapEnd) : overlappingRegions (t1Left ++ t1s') (t2Left ++ t2s')

part1 :: [Step] -> Int
part1 = filledArea

correctSteps :: [Step] -> [Step]
correctSteps = map correctStep
  where
    correctStep (Step _ _ color) =
      let rawDir = T.last color
          rawDist = "0x" ++ T.unpack (T.init color)
      in Step (parseDir rawDir) (read rawDist) color
      where
        parseDir '0' = East
        parseDir '1' = South
        parseDir '2' = West
        parseDir '3' = North

part2 :: [Step] -> Int
part2 steps =
  let steps' = correctSteps steps
  in filledArea steps'

main = do
  input <- TIO.readFile "input"
  let steps = parseSteps input
  print $ part1 steps
  print $ part2 steps
