module Day2 where

import Data.Char
import Data.List
import qualified Data.Map as Map

type CubeCounts = Map.Map String Int

data Game =
  Game { numId :: Int, pulls :: [CubeCounts] }
  deriving Show

parsePull :: String -> [(String, Int)]
parsePull "" = []
parsePull str =
  let
    (amount, rest') = span isDigit str
    (color, rest) = span (/= ',') (drop 1 rest')
  in [(color, read amount)] ++ parsePull (drop 2 rest)

parsePulls :: String -> [CubeCounts]
parsePulls "" = []
parsePulls str =
  let (pull, rest) = span (/= ';') str
  in [Map.fromList (parsePull pull)] ++ parsePulls (drop 2 rest)

parseGame :: String -> Game
parseGame str =
  let
    stripped = drop (length "Game ") str
    (numId, rest) = span isDigit stripped
  in Game (read numId) (parsePulls (drop 2 rest))

maxSeen :: [CubeCounts] -> CubeCounts
maxSeen = foldl (Map.unionWith max) Map.empty

validCount :: CubeCounts -> (String, Int) -> Bool
validCount realCounts (color, count) =
  let realCount = Map.findWithDefault 0 color realCounts
  in count <= realCount

validGame :: CubeCounts -> Game -> Bool
validGame realCounts game =
  let maxClaimedCounts = maxSeen (pulls game)
  in all (validCount realCounts) (Map.toList maxClaimedCounts)

part1 :: [Game] -> Int
part1 games =
  let
    realCounts = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]
    validGames = filter (validGame realCounts) games
  in foldl (+) 0 (map numId validGames)

power :: Game -> Int
power game =
  let maxClaimedCounts = maxSeen (pulls game)
  in Map.foldr (*) 1 maxClaimedCounts

part2 :: [Game] -> Int
part2 games =
  let powers = map power games
  in sum powers

main = do
  input <- readFile "input"
  let games = map parseGame (lines input)
  putStrLn $ show $ part1 games
  putStrLn $ show $ part2 games
