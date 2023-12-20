module Main where

import Data.Char (isNumber)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Operator = LessThan | GreaterThan deriving (Show)

data Target = Accept | Reject | Run T.Text deriving (Show)

data Rule = Rule {
  category :: Char,
  operator :: Operator,
  value :: Int,
  target :: Target
} deriving (Show)

data Workflow = Workflow {
  rules :: [Rule],
  fallback :: Target
} deriving (Show)

parseWorkflow :: T.Text -> (T.Text, Workflow)
parseWorkflow line =
  let [name, rulesText] = T.splitOn (T.pack "{") (T.dropEnd 1 line)
  in (name, parseWorkflowParts [] (T.splitOn (T.pack "," ) rulesText))
  where
    parseWorkflowParts rules [fallback] = Workflow rules (parseTarget fallback)
    parseWorkflowParts rules (rawRule:rest) = parseWorkflowParts (rules ++ [parseRule rawRule]) rest
    parseRule rawRule =
      let category = T.index rawRule 0
          operator = if T.index rawRule 1 == '<' then LessThan else GreaterThan
          value = read $ T.unpack $ T.dropAround (not . isNumber) rawRule
          target = parseTarget $ T.takeWhileEnd (/= ':') rawRule
      in Rule category operator value target
    parseTarget text
      | text == T.pack "A" = Accept
      | text == T.pack "R" = Reject
      | otherwise = Run text
type Workflows = M.Map T.Text Workflow

type Part = M.Map Char Int
parsePart :: T.Text -> Part
parsePart line =
  let rawRatings = T.splitOn (T.pack ",") $ T.drop 1 $ T.dropEnd 1 line
  in M.fromList $ map parseRating rawRatings
  where
    parseRating rawRating =
      let category = T.index rawRating 0
          value = read $ T.unpack $ T.drop 2 rawRating
      in (category, value)

parseInput :: T.Text -> (Workflows, [Part])
parseInput rawInput =
  let [rawWorkflows, rawParts] = map T.lines $ T.splitOn (T.pack "\n\n") rawInput
      workflows = M.fromList $ map parseWorkflow rawWorkflows
      parts = map parsePart rawParts
  in (workflows, parts)

evaluateRule :: Part -> Rule -> Maybe Target
evaluateRule part (Rule category operator value target) =
  let score = part M.! category
  in if evaluateRule' score operator value
    then Just target
    else Nothing
  where
    evaluateRule' score LessThan value = score < value
    evaluateRule' score GreaterThan value = score > value

evaluateWorkflow :: Part -> Workflow -> Target
evaluateWorkflow part (Workflow rules fallback) =
  let ruleResults = map (evaluateRule part) rules
  in fromMaybe fallback $ listToMaybe $ catMaybes ruleResults

isAccepted :: Workflows -> Part -> Bool
isAccepted workflows part = isAccepted' (T.pack "in")
  where
    isAccepted' workflowName =
      let workflow = workflows M.! workflowName
      in case evaluateWorkflow part workflow of
        Accept -> True
        Reject -> False
        Run nextWorkflow -> isAccepted' nextWorkflow

totalPartScore :: Part -> Int
totalPartScore = sum . M.elems

part1 :: Workflows -> [Part] -> Int
part1 workflows parts =
  let acceptedParts = filter (isAccepted workflows) parts
  in sum $ map totalPartScore acceptedParts

type QuantumPart = M.Map Char (Int, Int)
impossible :: QuantumPart -> Bool
impossible = any (uncurry (>)) . M.elems

quantifyRule :: QuantumPart -> Rule -> ((QuantumPart, Target), QuantumPart)
quantifyRule qPart (Rule category operator value target) =
  let (loValue, hiValue) = qPart M.! category
      (passedRange, failedRange) = case operator of
        LessThan -> ((loValue, value - 1), (value, hiValue))
        GreaterThan -> ((value + 1, hiValue), (loValue, value))
  in ((withRange passedRange, target), withRange failedRange)
  where
    withRange range = M.insert category range qPart

quantifyWorkflow :: QuantumPart -> Workflow -> [(QuantumPart, Target)]
quantifyWorkflow qPart (Workflow rules fallback) = quantifyWorkflow' qPart rules
  where
    quantifyWorkflow' qPart _ | impossible qPart = []
    quantifyWorkflow' qPart [] = [(qPart, fallback)]
    quantifyWorkflow' qPart (rule:rules) =
      let (passedOutcome, failedPart) = quantifyRule qPart rule
      in passedOutcome : quantifyWorkflow' failedPart rules

statesAccepted :: Workflows -> Int
statesAccepted workflows = 
  let startPart = M.fromList $ map (,(1,4000)) "xmas"
      startWorkflow = workflows M.! T.pack "in"
  in statesAccepted' startPart startWorkflow
  where
    statesAccepted' qPart workflow = sum $ map quantifyTarget $ quantifyWorkflow qPart workflow
    quantifyTarget (qPart, Accept) = product $ map (\(lo, hi) -> hi - lo + 1) $ M.elems qPart
    quantifyTarget (_,     Reject) = 0
    quantifyTarget (qPart, Run name) = statesAccepted' qPart (workflows M.! name)

part2 :: Workflows -> Int
part2 = statesAccepted

main = do
  input <- TIO.readFile "input"
  let (workflows, parts) = parseInput input
  print $ part1 workflows parts
  print $ part2 workflows