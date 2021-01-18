{-# LANGUAGE Strict #-}

module Hw4.BenchTask3
  ( -- * Function
    chtBench
  )
where

import Control.Concurrent.Async (mapConcurrently_)
import Criterion.Main (bench, bgroup, defaultMain, nfAppIO)
import Hw4.Task3 (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)
import System.Random (newStdGen, randomRs)

-- | Benchmark for testing the cht with the Criterion.
chtBench :: IO ()
chtBench =
  do
    rand <- newStdGen
    let randList = randomRs (0 :: Int, 1000000) rand
    cht <- newCHT
    cht2 <- newCHT
    defaultMain
      [ bgroup
          "Task3. CHT - Parallel perfom "
          [ bench "10^3" $
              nfAppIO
                (mapConcurrently_ (workerBench cht))
                (take 1000 randList),
            bench "10^4" $
              nfAppIO
                (mapConcurrently_ (workerBench cht2))
                (take 10000 randList)
          ],
        bgroup
          "Task3. CHT - Parallel consumer - producer"
          [ bench "10^5 operations" $
              nfAppIO
                (mapConcurrently_ (workerBenchInSameElem cht))
                (take 100000 randList)
          ]
      ]

-------------------------------------------
--         Auxiliary functions           --
-------------------------------------------

-- | A function to execute in a separate thread (for
-- the benchmark of parallel operations)
workerBench :: ConcurrentHashTable Int Int -> Int -> IO (Maybe Int)
workerBench table num =
  do
    putCHT num num table
    _ <- sizeCHT table
    getCHT num table

-- | A function to execute in a separate thread (for
-- the benchmark of consumer - producer execution)
workerBenchInSameElem :: ConcurrentHashTable Int Int -> Int -> IO ()
workerBenchInSameElem table num =
  do
    let e = num `mod` 3
        isProducer = even (num `mod` 7)
    if isProducer
      then do
        putCHT e e table
        pure ()
      else do
        _ <- getCHT e table
        pure ()
