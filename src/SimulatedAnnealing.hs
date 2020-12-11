module SimulatedAnnealing(
  anneal,
  Energy,
  EnergyFunction,
  NeighbourFunction
  )
where

import Prelude
import Data.List
import System.Random

type Temperature = Double
type Energy = Double
type NeighbourFunction c = (c, StdGen) -> (c, StdGen)
type AcceptanceProbability = Energy -> Energy -> Temperature -> Double
type EnergyFunction c = c -> Energy


acceptance_probability_SA :: AcceptanceProbability
acceptance_probability_SA e e' t
  | e' < e = 1
  | otherwise = exp $ - (e' - e) / t


temperatureFunction :: Temperature -> Temperature -> Int -> Int -> Temperature
temperatureFunction t0 tf nIterations n
  | t0 == 0 = 0
  | t0 < tf = tf
  | otherwise = t0 * (a ** (fromIntegral n))
  where
    a = exp $ log(tf/t0) / (fromIntegral nIterations) -- since log(tf/t0) = nIterations log(a)


anneal_tick :: EnergyFunction candidate -> NeighbourFunction candidate -> (candidate, StdGen) -> Temperature -> (candidate, StdGen)
anneal_tick ef nf (c, r) temp
  | acceptance_probability_SA (ef c) (ef c') temp >= p = (c', r'')
  | otherwise = (c, r'')
  where
    (c', r') = nf (c, r)
    (p, r'') = randomR (0, 1) r'


anneal :: candidate -> EnergyFunction candidate -> NeighbourFunction candidate -> Temperature -> Temperature -> Int -> candidate
anneal c0 ef nf t0 tf nIterations
  = fst $ foldl' (anneal_tick ef nf) (c0, r) $ reverse temperatures
  where
    temperatures = fmap (temperatureFunction t0 tf nIterations) [0..nIterations] :: [Temperature]
    r = (mkStdGen 42)
