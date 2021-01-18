module Hw4.BenchTask2
  ( -- * Function
    monteCarloBench
  )
where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Hw4.Task2 (lineMonteCarlo, parMonteCarlo)

-- | Some function for integration
functionForInt :: (Floating a) => a -> a
functionForInt x = 1 / tan (x ** 2) - cos x

-- | Benchmark for testing the MonteCarlo function with the Criterion
monteCarloBench :: IO ()
monteCarloBench =
  do
    defaultMain
      [ bgroup
          "Task2. MonteCarlo - Line "
          [ bench "10^6" $
              whnf
                (lineMonteCarlo 1000000 functionForInt)
                (0, 1.5 :: Double)
          ],
        bgroup
          "Task2. MonteCarlo - Parallel "
          [ bench "10^6" $
              whnf
                (parMonteCarlo 1000000 functionForInt) 
                (0, 1.5 :: Double)
          ]
      ]
