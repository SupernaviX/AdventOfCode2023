module Main where

import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex func = toList . S.mapWithIndex func . S.fromList

type Position = (Int, Int)
data Rock = Round | Square deriving (Ord, Eq, Show)
data Grid = Grid { rocks :: M.Map Position Rock, width :: Int, height :: Int } deriving (Ord, Eq)

showGrid :: Grid -> String
showGrid grid = concatMap showRow [0..height grid - 1]
  where
    showRow y = map (`showCell` y) [0..width grid - 1] ++ "\n"
    showCell x y = showCell' (M.lookup (x, y) (rocks grid))
    showCell' Nothing = '.'
    showCell' (Just Square) = '#'
    showCell' (Just Round) = 'O'

instance Show Grid where show = showGrid

data Direction = North | South | East | West deriving (Eq, Ord, Enum, Show)

parseRocks :: Int -> T.Text -> [(Position, Rock)]
parseRocks y rocks = catMaybes $ mapWithIndex parseRock (T.unpack rocks)
  where
    parseRock _ '.' = Nothing
    parseRock x '#' = Just ((x, y), Square)
    parseRock x 'O' = Just ((x, y), Round)

parseGrid :: T.Text -> Grid
parseGrid text =
  let rows@(firstRow:_) = T.lines text
      height = length rows
      width = T.length firstRow
      innerRocks = concat $ mapWithIndex parseRocks rows
      hBorders = concat [[(-1, y), (width, y)] | y <- [0..height - 1]]
      vBorders = concat [[(x, -1), (x, height)] | x <- [0..width - 1]]
      borderRocks = map (, Square) (hBorders ++ vBorders)
      rocks = M.fromList (innerRocks ++ borderRocks)
  in Grid rocks width height

slide :: Direction -> Grid -> Grid
slide dir grid = 
  let allLines = linesInDir dir grid
      slidLines = map (slideLine (nextFree dir)) allLines
      newRocks = M.fromList $ concat slidLines
  in Grid newRocks (width grid) (height grid)
  where
    nextFree North (x, y) = (x, y + 1)
    nextFree South (x, y) = (x, y - 1)
    nextFree West (x, y) = (x + 1, y)
    nextFree East (x, y) = (x - 1, y)

linesInDir :: Direction -> Grid -> [[(Position, Rock)]]
linesInDir North grid = linesByCoord fst grid
linesInDir South grid = map reverse $ linesByCoord fst grid
linesInDir West grid = linesByCoord snd grid
linesInDir East grid = map reverse $ linesByCoord snd grid

linesByCoord :: (Position -> Int) -> Grid -> [[(Position, Rock)]]
linesByCoord coord grid =
  let allRocks = M.toList (rocks grid)
  in M.elems $ groupByCoord allRocks
  where
    groupByCoord = foldr addToMap M.empty
    addToMap rock = M.insertWith (++) (coord (fst rock)) [rock]

slideLine :: (Position -> Position) -> [(Position, Rock)] -> [(Position, Rock)]
slideLine nextFree (border: rocks) =
  scanl nextRock border rocks
  where
    nextRock _ (pos, Square) = (pos, Square)
    nextRock (pos, _) (_, Round) = (nextFree pos, Round)

northWeight :: Grid -> Int
northWeight grid =
  let allRocks = M.toList (rocks grid)
  in sum (map rockWeight allRocks)
  where
    rockWeight (_, Square) = 0
    rockWeight ((_, y), Round) = height grid - y

spin :: Grid -> Grid
spin = slide East . slide South . slide West . slide North

spinBillion :: Grid -> Grid
spinBillion grid =
  let (grid', offset, cycles) = findCycle grid
      cyclesLeft = mod (1000000000 - offset) cycles
      finalGrid:_ = drop cyclesLeft $ iterate spin grid'
  in finalGrid

findCycle :: Grid -> (Grid, Int, Int)
findCycle = findCycle' M.empty 0
findCycle' seen cycles grid =
  case M.lookup grid seen of
    Just offset -> (grid, offset, cycles - offset)
    Nothing -> findCycle' (M.insert grid cycles seen) (cycles + 1) (spin grid)

part1 :: Grid -> Int
part1 grid =
  let slid = slide North grid
  in northWeight slid

part2 :: Grid -> Int
part2 grid =
  let spun = spinBillion grid
  in northWeight spun

main = do
  input <- TIO.readFile "input"
  let grid = parseGrid input
  print $ part1 grid
  print $ part2 grid