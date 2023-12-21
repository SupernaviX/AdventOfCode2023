module Main where

import Control.Exception (assert)
import Data.Bits (shiftL)
import Data.List (sortBy, groupBy, find)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

toGroups :: Ord k => Ord v => [(k, v)] -> [(k, [v])]
toGroups pairs =
  let groups = groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> compare (fst a) (fst b)) pairs
  in map (\g@((key, _):_) -> (key, map snd g)) groups

data ModuleType = Broadcaster | FlipFlop | Conjunction | Receiver deriving (Eq, Show)

data Modules = Modules {
  types :: M.Map T.Text ModuleType,
  forwards :: M.Map T.Text [T.Text],
  backwards :: M.Map T.Text [T.Text]
} deriving (Show)

parseModules :: T.Text -> Modules
parseModules text =
  let allModules = ((T.pack "rx", Receiver), []) : map parseModule (T.lines text)
      moduleTypes = map fst allModules
      forwardConns = concatMap snd allModules
      backwardConns = map (\(a, b) -> (b, a)) forwardConns
  in Modules (M.fromList moduleTypes) (toMap forwardConns) (toMap backwardConns)
  where
    toMap = M.fromList . toGroups

parseModule :: T.Text -> ((T.Text, ModuleType), [(T.Text, T.Text)])
parseModule text
  | T.index text 0 == '%'                    = parseModule' (T.drop 1 text) FlipFlop
  | T.index text 0 == '&'                    = parseModule' (T.drop 1 text) Conjunction
  | T.isPrefixOf (T.pack "broadcaster") text = parseModule' text Broadcaster
  where
    parseModule' str typ =
      let [name, rawOutputs] = T.splitOn (T.pack " -> ") str
          outputs = T.splitOn (T.pack ", ") rawOutputs
      in ((name, typ), map (name,) outputs)

data Counter = Counter {
  fireAt :: Int,
  resetTo :: Int
} deriving (Show)

findCounters :: Modules -> [Counter]
findCounters modules =
  let lowBits = forwards modules M.! T.pack "broadcaster"
  in map (findCounter modules) lowBits

findCounter :: Modules -> T.Text -> Counter
findCounter modules lowBit = findCounter' [lowBit]
  where
    isBit name = types modules M.! name == FlipFlop
    findCounter' bits@(bit:_) =
      let
        nextModules = forwards modules M.! bit
        nextBit = find isBit nextModules
      in case nextBit of
        Just aBit -> findCounter' (aBit : bits)
        Nothing ->
          let [conj] = nextModules
          in toCounter modules conj bits

toCounter :: Modules -> T.Text -> [T.Text] -> Counter
toCounter modules conj bits =
  let fireAtBits = bitsWithValues (backwards modules M.! conj)
      resetOutputs = filter (`elem` bits) (forwards modules M.! conj)
      resetToBits = computeReset fireAtBits resetOutputs
  in Counter (bitsToNumber fireAtBits) (bitsToNumber resetToBits)
  where
    bitsWithValues values = Seq.fromList $ map (\bit -> (bit, bit `elem` values)) bits
    bitsToNumber bits = sum $ Seq.mapWithIndex (\idx (_, val) -> if val then shiftL 1 idx else 0) $ Seq.reverse bits

computeReset :: Seq.Seq (T.Text, Bool) -> [T.Text] -> Seq.Seq (T.Text, Bool)
computeReset values [] = values
computeReset values (flipped:flips) =
  let Just flippedIndex = Seq.findIndexL (\(bit, _) -> bit == flipped) values
      oldValue = snd (Seq.index values flippedIndex)
      values' = Seq.update flippedIndex (flipped, not oldValue) values
      flips' = if oldValue && flippedIndex > 0 then flips ++ [fst (Seq.index values' (flippedIndex - 1))] else flips
  in computeReset values' flips'

part2 :: Modules -> Int
part2 modules =
  let counters = map (\c -> assert (resetTo c == 0) c) $ findCounters modules
  in foldr (lcm . fireAt) 1 counters

main = do
  input <- TIO.readFile "input"
  let modules = parseModules input
  print $ part2 modules
