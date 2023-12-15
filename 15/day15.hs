module Main where

import Data.Char (digitToInt, ord)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

findInstructions :: T.Text -> [T.Text]
findInstructions = T.splitOn (T.pack ",") . T.strip

hash :: T.Text -> Int
hash = foldr hash' 0 . T.unpack . T.reverse
  where
    hash' char curr = mod ((curr + ord char) * 17) 256

part1 :: [T.Text] -> Int
part1 = sum . map hash

data Instruction = Add T.Text Int | Remove T.Text deriving (Show)
parseInstruction :: T.Text -> Instruction
parseInstruction text
  | T.isSuffixOf (T.pack "-") text = parseRemove text
  | otherwise = parseAdd text

labelHash :: Instruction -> Int
labelHash = hash . label
  where label (Add x _) = x
        label (Remove x) = x

parseAdd :: T.Text -> Instruction
parseAdd text =
  let label = T.take (T.length text - 2) text
      focal = digitToInt $ T.last text
  in Add label focal

parseRemove :: T.Text -> Instruction
parseRemove text =
  let label = T.take (T.length text - 1) text
  in Remove label

data Slot = Slot T.Text Int deriving (Show)
type Box = S.Seq Slot
type Boxes = S.Seq Box

doInstructions :: [Instruction] -> Boxes -> Boxes
doInstructions instrs boxes =
  foldr doInstruction boxes (reverse instrs)

doInstruction :: Instruction -> Boxes -> Boxes
doInstruction instr = S.adjust' (doInstr' instr) (labelHash instr)
  where doInstr' (Add label focal) = doAdd label focal
        doInstr' (Remove label) = doRemove label

doAdd :: T.Text -> Int -> Box -> Box
doAdd label focal box =
  let oldIndex = S.findIndexL (\(Slot l _) -> l == label) box
      newSlot = Slot label focal
  in case oldIndex of
    Just i -> S.update i newSlot box
    Nothing -> box S.|> newSlot

doRemove :: T.Text -> Box -> Box
doRemove label = S.filter (\(Slot l _) -> l /= label)

focusingPower :: Boxes -> Int
focusingPower = sum . S.mapWithIndex boxPower
  where
    boxPower boxIndex = sum . S.mapWithIndex (lensPower boxIndex)
    lensPower boxIndex lensIndex (Slot _ focal) = (1 + boxIndex) * (1 + lensIndex) * focal

part2 :: [T.Text] -> Int
part2 rawInstrs = 
  let instructions = map parseInstruction rawInstrs
      initBoxes = S.fromList $ replicate 256 S.empty
      finalBoxes = doInstructions instructions initBoxes
  in focusingPower finalBoxes

main = do
  input <- TIO.readFile "input"
  let instructions = findInstructions input
  print $ part1 instructions
  print $ part2 instructions
