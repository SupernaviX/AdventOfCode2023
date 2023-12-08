module Main where

import Data.Foldable (toList)
import Data.List (elemIndex, group, partition, sort, sortOn)
import Data.Maybe (fromJust)
import Data.Sequence (fromList, mapWithIndex)
import qualified Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bifunctor

data Card = Ace | King | Queen | Jack | Ten |
            Nine | Eight | Seven | Six | Five |
            Four | Three | Two | Joker
  deriving (Eq, Ord, Enum, Show)

data HandType = FiveOfAKind | FourOfAKind | FullHouse |
                ThreeOfAKind | TwoPair | OnePair | HighCard
  deriving (Eq, Ord, Show)

parseHand :: T.Text -> [Card]
parseHand hand = map parseCard (T.unpack hand)
  where parseCard card = toEnum $ fromJust $ elemIndex card "AKQJT98765432"

parseLine :: T.Text -> ([Card], Int)
parseLine line =
  let [hand, bid] = T.words line
  in (parseHand hand, (read . T.unpack) bid)

typeFromCounts :: [Int] -> HandType
typeFromCounts = typeFromCounts' . sort
typeFromCounts' [5] = FiveOfAKind
typeFromCounts' [1, 4] = FourOfAKind
typeFromCounts' [2, 3] = FullHouse
typeFromCounts' [1, 1, 3] = ThreeOfAKind
typeFromCounts' [1, 2, 2] = TwoPair
typeFromCounts' [1, 1, 1, 2] = OnePair
typeFromCounts' [1, 1, 1, 1, 1] = HighCard

incrementAt :: Int -> [Int] -> [Int]
incrementAt i list =
  let (before,at:after) = splitAt i list
  in before ++ (at + 1) : after

-- Given counts of distinct cards (in a hand), and a count of jokers,
-- return any possible counts of distinct cards you could get with those jokers
applyJokers :: [Int] -> Int -> [[Int]]
applyJokers [] 5 = [[5]] -- Five jokers always gets you five-of-a-kind
applyJokers counts 0 = [counts] -- adding 0 jokers does nothing
applyJokers counts jokers =
  let allCounts = [applyJokers (incrementAt i counts) (jokers - 1) | i <- [0..length counts - 1]]
  in concat allCounts

findType :: [Card] -> HandType
findType hand =
  let (jokers, other) = partition (== Joker) hand
      otherCounts = map length $ group (sort other)
      possibleCounts = applyJokers otherCounts (length jokers)
      possibleTypes = map typeFromCounts possibleCounts
  in minimum possibleTypes

computeWinnings :: [([Card], Int)] -> Int
computeWinnings hands =
  let sorted = sortOn (Data.Ord.Down . rankHand . fst) hands
      scores = mapWithIndex (\rank (_, bid) -> (1 + rank) * bid) (fromList sorted)
  in sum (toList scores)
  where rankHand cards = (findType cards, cards)

jokerfyCard :: Card -> Card
jokerfyCard c = if c == Jack then Joker else c
jokerfyHand :: [Card] -> [Card]
jokerfyHand = map jokerfyCard

part1 :: [([Card], Int)] -> Int
part1 = computeWinnings

part2 :: [([Card], Int)] -> Int
part2 hands =
  let jokerfiedHands = map (Data.Bifunctor.first jokerfyHand) hands
  in computeWinnings jokerfiedHands

main = do
  input <- TIO.readFile "input"
  let hands = map parseLine (T.lines input)
  let jokered = map (jokerfyHand . fst) hands
  print $ part1 hands
  print $ part2 hands
