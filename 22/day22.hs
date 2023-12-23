module Main where

import Data.Function (fix)
import Data.List (foldl', groupBy, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

toGroups :: Ord a => Ord b => [(a, b)] -> [(a, [b])]
toGroups pairs = map toGroup $ groupBy (\a b -> fst a == fst b) $ sort pairs
  where toGroup pairs@((key, val):_) = (key, map snd pairs)

type Range = (Int, Int)
data Brick = Brick {
  xRange :: Range,
  yRange :: Range,
  zRange :: Range
} deriving (Show)

parseBricks :: T.Text -> [(Int, Brick)]
parseBricks = addId . sortBy compareHeight . map parseBrick . T.lines
  where
    addId = zip [0..]
    compareHeight brick1 brick2 = compare (zRange brick1) (zRange brick2)
    parseBrick line =
      let [(x1, y1, z1), (x2, y2, z2)] = map parseVec $ T.splitOn (T.pack "~") line
      in Brick (x1, x2) (y1, y2) (z1, z2)
    parseVec vec =
      let [x, y, z] = map (read . T.unpack) (T.splitOn (T.pack ",") vec)
      in (x, y, z)

type BrickSlice = (Int, Range, Range)
type BrickHeap = Seq.Seq [BrickSlice]

findSoleSupporters :: [(Int, Brick)] -> [(Int, Int)]
findSoleSupporters bricks = concatMap soleSupporter (toGroups $ findSupporters bricks)
  where
    soleSupporter (id, [supporterId]) = [(id, supporterId)]
    soleSupporter _ = []

findSupporterMap :: [(Int, Brick)] -> M.Map Int [Int]
findSupporterMap = M.fromList . toGroups . findSupporters

findSupporters :: [(Int, Brick)] -> [(Int, Int)]
findSupporters bricks =
  let startHeap = Seq.fromList [[(-1, (0, 9), (0, 9))]]
      (_, supporters) = foldl' dropBrick (startHeap, []) bricks
  in filter (\(_, dep) -> dep >= 0) supporters

dropBrick :: (BrickHeap, [(Int, Int)]) -> (Int, Brick) -> (BrickHeap, [(Int, Int)])
dropBrick (bricks, supporters) (id, Brick xRange yRange zRange) =
  let zBelow = fromJust $ Seq.findIndexR (any overlapsRanges) bricks
      newSupporters = map (\(supporterId, _, _) -> (id, supporterId)) $ filter overlapsRanges (Seq.index bricks zBelow)
      zHeight = snd zRange - fst zRange + 1
      zs = [zBelow + 1 .. zBelow + zHeight]
      bricks' = withBrick bricks (id, xRange, yRange) zs
      supporters' = supporters ++ newSupporters
  in (bricks', supporters')
  where
    overlapsRanges (_, (x1, x2), (y1, y2)) =
      snd xRange >= x1 && x2 >= fst xRange && snd yRange >= y1 && y2 >= fst yRange
    withBrick heap _ [] = heap
    withBrick heap slice (z:zs) = withBrick (addSlice heap slice z) slice zs
    addSlice heap slice z | z < Seq.length heap = Seq.adjust' (slice:) z heap
    addSlice heap slice z = heap Seq.|> [slice]

part1 :: [(Int, Brick)] -> Int
part1 bricks =
  let allIds = map fst bricks
      soleSupporters = findSoleSupporters bricks
      unessential = S.difference (S.fromList allIds) (S.fromList (map snd soleSupporters))
  in S.size unessential

type KeystoneFinder = Int -> S.Set Int
keystones :: M.Map Int [Int] -> KeystoneFinder -> KeystoneFinder
keystones supporterMap f id =
  let supporters = map f $ M.findWithDefault [] id supporterMap
  in if null supporters
    then S.singleton id
    else S.union (S.singleton id) (foldr1 S.intersection supporters)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0..] !!)

findKeystones :: M.Map Int [Int] -> KeystoneFinder
findKeystones supporters = fix (memoize . keystones supporters)

part2 :: [(Int, Brick)] -> Int
part2 bricks =
  let allIds = map fst bricks
      supporters = findSupporterMap bricks
      keystones = map (findKeystones supporters) allIds
  in sum $ map (\set -> S.size set - 1) keystones

main = do
  input <- TIO.readFile "input"
  let bricks = parseBricks input
  print $ part1 bricks
  print $ part2 bricks
