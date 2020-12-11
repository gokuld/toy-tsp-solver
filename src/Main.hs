import Prelude

import System.Random

import qualified SimulatedAnnealing as SA
import qualified TSP as TSP

generate_random_tour_data :: StdGen -> Double -> Double -> Double -> Double -> Int -> TSP.Tour
generate_random_tour_data r xMin xMax yMin yMax n
  | n <= 0 = []
  | xMax < xMin || yMax < yMin = []
  | otherwise = ((x, y)::TSP.Point) : generate_random_tour_data r'' xMin xMax yMin yMax (n - 1)
  where
    (x, r') = randomR (xMin, xMax) r
    (y, r'') = randomR (yMin, yMax) r'

main :: IO ()
main = do
  putStrLn $ "Initial candidate: " ++ "show c0" ++ " with energy " ++ show (TSP.energy c0)
  putStrLn $ "Final candidate: " ++ "show c'" ++ " with energy " ++ show (TSP.energy c')
  putStrLn $ "(After " ++ show n_iterations ++ " iterations)."
  where
    n_iterations = 100000 :: Int
    t0 = 1 -- initial temperature
    tf = 1e-6 -- final temperature
    (xMin, yMin, xMax, yMax) = ((-1), 1, (-3), 3)
    c0 = generate_random_tour_data (mkStdGen 42) xMin yMin xMax yMax 100
    c' = SA.anneal c0 TSP.energy TSP.twoOptRandom t0 tf n_iterations
