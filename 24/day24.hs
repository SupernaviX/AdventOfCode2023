module Main where

import qualified Data.Decimal as D
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Vec3 = Vec3 {
  x :: D.Decimal,
  y :: D.Decimal,
  z :: D.Decimal
} deriving (Show)

v3map :: (D.Decimal -> D.Decimal) -> Vec3 -> Vec3
v3map with (Vec3 x y z) = Vec3 (with x) (with y) (with z)
v3zip :: (D.Decimal -> D.Decimal -> D.Decimal) -> Vec3 -> Vec3 -> Vec3
v3zip with (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (with x1 x2) (with y1 y2) (with z1 z2)

v3add :: Vec3 -> Vec3 -> Vec3
v3add = v3zip (+)
v3sub :: Vec3 -> Vec3 -> Vec3
v3sub = v3zip (-)
v3mul :: Vec3 -> D.Decimal -> Vec3
v3mul vec3 by = v3map (* by) vec3
v3div :: Vec3 -> D.Decimal -> Vec3
v3div vec3 by = v3map (/ by) vec3

v3cross :: Vec3 -> Vec3 -> Vec3
v3cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  let x' = y1*z2 - z1*y2
      y' = z1*x2 - x1*z2
      z' = x1*y2 - y1*x2
  in Vec3 x' y' z'

data Hailstone = Hailstone {
  position :: Vec3,
  velocity :: Vec3
} deriving (Show)

parseHailstones :: T.Text -> [Hailstone]
parseHailstones text = map parseHailstone $ T.lines text
  where
    parseHailstone line =
      let [position, velocity] = map parseVector $ T.splitOn (T.pack " @ ") line
      in Hailstone position velocity
    parseVector chunk =
      let [x, y, z] = map (read . T.unpack) $ T.splitOn (T.pack ", ") chunk
      in Vec3 x y z

type LinEqVars = (D.Decimal, D.Decimal, D.Decimal)

findLinEqVars :: Hailstone -> LinEqVars
findLinEqVars (Hailstone (Vec3 x1 y1 _) (Vec3 dx1 dy1 _)) =
  let a = dy1
      b = -dx1
      c = y1 * dx1 - dy1 * x1
  in (a, b, c)

solveLinEq :: LinEqVars -> LinEqVars -> Maybe (D.Decimal, D.Decimal)
solveLinEq (a1, b1, c1) (a2, b2, c2) =
  let cancelA1Factor = -a1 / a2
      y = -(c1 + (c2 * cancelA1Factor)) / (b1 + (b2 * cancelA1Factor))
      cancelB1Factor = -b1 / b2
      x = -(c1 + (c2 * cancelB1Factor)) / (a1 + (a2 * cancelB1Factor))
  in if cancelA1Factor == cancelB1Factor then Nothing else Just (x, y)

inPast :: Hailstone -> (D.Decimal, D.Decimal) -> Bool
inPast (Hailstone (Vec3 xStart _ _) (Vec3 dx _ _)) (x, y) = signum (x - xStart) /= signum dx

find2dIntersection :: Hailstone -> Hailstone -> Maybe (D.Decimal, D.Decimal)
find2dIntersection h1 h2 = do
  solution <- solveLinEq (findLinEqVars h1) (findLinEqVars h2)
  if inPast h1 solution || inPast h2 solution
    then Nothing
    else Just solution

isIntersectionInRange :: Hailstone -> Hailstone -> (D.Decimal, D.Decimal) -> Bool
isIntersectionInRange h1 h2 (minAllowed, maxAllowed) =
  case find2dIntersection h1 h2 of
    Just (x, y) -> x >= minAllowed && x <= maxAllowed && y >= minAllowed && y <= maxAllowed
    Nothing -> False

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

part1 :: [Hailstone] -> (D.Decimal, D.Decimal) -> Int
part1 hailstones range =
  length $ filter (\(h1, h2) -> isIntersectionInRange h1 h2 range) (pairs hailstones)

parallel :: Hailstone -> Hailstone -> Bool
parallel h1 h2 =
  let (Vec3 dx1 dy1 dz1) = velocity h1
      (Vec3 dx2 dy2 dz2) = velocity h2
  in  dx1 / dy1 == dx2 / dy2 &&
      dx1 / dz1 == dx2 / dz2 &&
      dy1 / dz1 == dy2 / dz2

findNonParallel :: [Hailstone] -> [Hailstone]
findNonParallel = foldl addNonParallel []
  where
    addNonParallel found hailstone =
      if any (parallel hailstone) found
        then found
        else hailstone : found

normalize :: Hailstone -> Hailstone -> Hailstone
normalize (Hailstone refPosition refVelocity) (Hailstone position velocity) =
  Hailstone (v3sub position refPosition) (v3sub velocity refVelocity)

type PlaneEqVars = (D.Decimal, D.Decimal, D.Decimal, D.Decimal)
-- find the equation for a plane formed by the origin and two points on the line of the given hailstone.
-- (this is meaningful because the hailstone has been normalized with respect to another hailstone)
findPlaneEquation :: Hailstone -> PlaneEqVars
findPlaneEquation (Hailstone position velocity) =
  let Vec3 a b c = v3cross velocity position
  in (a, b, c, 0)

findPlaneIntersection :: PlaneEqVars -> Hailstone -> (Vec3, D.Decimal)
findPlaneIntersection (a, b, c, d) (Hailstone (Vec3 x0 y0 z0) (Vec3 dx dy dz)) =
  let t = -(a*x0 + b*y0 + c*z0 + d) / (a*dx + b*dy + c*dz)
      pos = Vec3 (x0 + t*dx) (y0 + t*dy) (z0 + t*dz)
  in (pos, t)

findVelocity :: (Vec3, D.Decimal) -> (Vec3, D.Decimal) -> Vec3
findVelocity (pos1, t1) (pos2, t2) = v3div (v3sub pos2 pos1) (t2 - t1)

findOrigin :: Vec3 -> Hailstone -> D.Decimal -> Vec3
findOrigin rockVelocity (Hailstone hOrigin hVelocity) t =
  let hPosition = v3add hOrigin (v3mul hVelocity t)
  in v3sub hPosition (v3mul rockVelocity t)

--part2 :: [Hailstone] -> Int
part2 hailstones =
  let [h1, h2, h3, h4] = take 4 $ findNonParallel hailstones
      [h2', h3', h4'] = map (normalize h1) [h2, h3, h4]
      plane = findPlaneEquation h2'
      (pos1, t1) = findPlaneIntersection plane h3'
      (pos2, t2) = findPlaneIntersection plane h4'
      rockVelocity = v3add (velocity h1) (findVelocity (pos1, t1) (pos2, t2))
      rockOrigin = findOrigin rockVelocity h3 t1
  in x rockOrigin + y rockOrigin + z rockOrigin

-- Find four non-parallel hailstones.
-- Normalize all positions + velocities relative to the first.
-- Choose two points from the line described by the second hailstone. those two points, plus the origin, form a plane.
-- Find the location of two points where hailstones 3 and 4 intersect that plane. You can now find exactly where and when the rock was at two separate points.
-- From there, just figure out where it was at t=0 and bob's your uncle.


main = do
  putStrLn "started"
  input <- TIO.readFile "input"
  let hailstones = parseHailstones input
  print $ part1 hailstones (200000000000000, 400000000000000)
  print $ part2 hailstones
