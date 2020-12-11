module TSP (
  Point,
  Tour,
  energy,
  twoOptRandom
  )
where

import System.Random
import Data.List

import SimulatedAnnealing (Energy)

type Point = (Double, Double)
type Metric = Double
type Tour = [Point]
type TourMetric = Tour -> Metric


euclideanDistance :: Point -> Point -> Double
euclideanDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2


distanceMetric :: TourMetric
distanceMetric [] = 0
distanceMetric [_] = 0
distanceMetric (p0:p1:ps) = euclideanDistance p0 p1 + distanceMetric (p1:ps)

tourMetrics :: [TourMetric]
tourMetrics = [distanceMetric]


energy :: Tour -> Energy
energy t = sum $ tourMetrics  <*> [t]


------------
-- Moves. --
------------


{-|
Perform a two-opt move on a tour given two link ids and a tour.
A link id is an integer 'l' which describes a link from positions
'l' to 'l + 1'.

Results are not guaranteed to be correct if the link numbers l1 and l2
are out of their valid range of [0, length t]
-}
twoOpt :: Int -> Int -> [a] -> [a]
twoOpt l1 l2 t
  | l1 == l2 = t
  | l2 == l1 + 1 = t
  | l1 > l2 = twoOpt l2 l1 t
  | otherwise = let
      list1 = take l1 t
      list2 = (drop l1 . take l2) t
      list3 = drop l2 t
    in
      list1 ++ reverse list2 ++ list3


{-|
Perform a two-opt move on a tour with a randomly chosen pair of links.
-}
twoOptRandom :: ([a], StdGen) -> ([a], StdGen)
twoOptRandom (t, r) = (twoOpt l1 l2 t, r'')
  where
    len = length t
    (l1, r') = randomR ((0, len - 2)::(Int, Int)) r
    (l2, r'') = randomR ((l1 + 1, len - 1)::(Int, Int)) r'
