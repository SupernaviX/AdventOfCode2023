module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (sortOn)

newtype AlmanacList = AlmanacList { entries :: [((Int, Int), Int)] } deriving Show

parseSsv :: T.Text -> [Int]
parseSsv = map (read . T.unpack) . T.words

parseAlmanacRow :: T.Text -> ((Int, Int), Int)
parseAlmanacRow row =
  let [dstStart, srcStart, len] = parseSsv row
  in ((srcStart, srcStart + len), dstStart - srcStart)

parseAlmanacList :: T.Text -> AlmanacList
parseAlmanacList section =
  let
    (_ : rows) = T.lines section
    parsedRows = map parseAlmanacRow rows
  in AlmanacList (sortOn fst parsedRows)

parseInput :: T.Text -> ([Int], [AlmanacList])
parseInput lines =
  let
    (seedLine : sections) = T.splitOn (T.pack "\n\n") lines
    seeds = parseSsv (T.drop (length "seeds: ") seedLine)
    almanacLists = map parseAlmanacList sections
  in (seeds, almanacLists)

lookupInAlmanac :: AlmanacList -> Int -> Int
lookupInAlmanac (AlmanacList entries) src =
  let
    rule = filter (\((from, to), _) -> from <= src && src <= to) entries
    offset = maybe 0 snd (listToMaybe rule)
  in src + offset

-- Find the range(s) which (srcFrom, srcTo) are transformed to by these rules.
-- All the rules are assumed to at least partially overlap with the range.
-- Returned ranges may have negative size, the caller must filter them.
translateRange :: (Int, Int) -> [((Int, Int), Int)] -> [(Int, Int)]
translateRange (srcFrom, srcTo) [] = [(srcFrom, srcTo)]
translateRange (srcFrom, srcTo) (((from, to), offset) : rest) =
  let
    mappedRanges =
      [ (srcFrom, from - 1) -- any part of the source range before the transformation doesn't move
      , (offset + max from srcFrom, offset + min to srcTo) ] -- move the overlapping range as needed
  in mappedRanges ++ translateRange (to, srcTo) rest

lookupRangeInAlmanac :: AlmanacList -> (Int, Int) -> [(Int, Int)]
lookupRangeInAlmanac (AlmanacList entries) (srcFrom, srcTo) =
  let
    rules = filter (\((from, to), _) -> from <= srcTo && srcFrom <= to) entries
    newRanges = translateRange (srcFrom, srcTo) rules
  in filter (uncurry (<=)) newRanges

lookupRangesInAlmanac :: AlmanacList -> [(Int, Int)] -> [(Int, Int)]
lookupRangesInAlmanac list = concatMap (lookupRangeInAlmanac list)

toSeedRanges :: [Int] -> [(Int, Int)]
toSeedRanges [] = []
toSeedRanges (first : second : rest) = (first, first + second - 1) : toSeedRanges rest

part1 :: ([Int], [AlmanacList]) -> Int
part1 (seeds, almanacLists) =
  let locationNumbers = map (\i -> foldr lookupInAlmanac i (reverse almanacLists)) seeds
  in minimum locationNumbers

part2 :: ([Int], [AlmanacList]) -> Int
part2 (seedDefs, almanacLists) =
  let
    seedRanges = toSeedRanges seedDefs
    locationNumbers = foldr lookupRangesInAlmanac seedRanges (reverse almanacLists)
  in minimum (map fst locationNumbers)

main = do
  input <- TIO.readFile "input"
  let parsed = parseInput input
  print $ part1 parsed
  print $ part2 parsed
