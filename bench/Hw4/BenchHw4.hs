module Hw4.BenchHw4
  ( -- * Function
    benchHw4
  )
where

import Hw4.BenchTask1 (perimeterAndAreaBench)
import Hw4.BenchTask2 (monteCarloBench)
import Hw4.BenchTask3 (chtBench)

-- | Function to run benchmarks for Hw4
benchHw4 :: IO ()
benchHw4 =
  do
    perimeterAndAreaBench
    monteCarloBench
    chtBench
