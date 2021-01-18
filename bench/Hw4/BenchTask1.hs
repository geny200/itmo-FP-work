{-# LANGUAGE BangPatterns #-}

module Hw4.BenchTask1
  ( -- * Function
    perimeterAndAreaBench
  )
where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Hw4.Task1 (Point (..), crossProduct, distance, doubleArea, perimeter, plus)

-- | Function to run the benchmark
perimeterAndAreaBench :: IO ()
perimeterAndAreaBench =
  do
    perimeterBench
    doubleAreaBench

-- | Benchmark for testing the doubleArea function with the Criterion
doubleAreaBench :: IO ()
doubleAreaBench =
  defaultMain
    [ bgroup
        "Task1. doubleArea - Naive"
        [bench "10^7" $ whnf doubleAreaNaive (createFigure 10000000 [])],
      bgroup
        "Task1. doubleArea - Fust"
        [bench "10^7" $ whnf doubleArea (createFigure 10000000 [])]
    ]

-- | Benchmark for testing the perimeter function with the Criterion
perimeterBench :: IO ()
perimeterBench =
  defaultMain
    [ bgroup
        "Task1. perimeter - Naive"
        [bench "10^7" $ whnf perimeterNaive (createFigure 10000000 [])],
      bgroup
        "Task1. perimeter - Fust"
        [bench "10^7" $ whnf perimeter (createFigure 10000000 [])]
    ]

-- | A simpler (naive) implementation of the perimeter function
perimeterNaive :: [Point] -> Double
perimeterNaive [] = 0
perimeterNaive [_] = 0
perimeterNaive points =
  sum
    ( distance (last points) (head points) :
      zipWith distance points (tail points)
    )

-- | A simpler (naive) implementation of the doubleArea function
doubleAreaNaive :: [Point] -> Integer
doubleAreaNaive [] = 0
doubleAreaNaive [_] = 0
doubleAreaNaive [_, _] = 0
doubleAreaNaive points =
  sum
    ( crossProduct (last points) (head points) :
      zipWith crossProduct points (tail points)
    )

-- | Returns a point (1,0) or (0,1) depending on the parity of the number.
addPoint :: Integer -> Point
addPoint size =
  if even size
    then Point 0 1
    else Point 1 0

-- | A function for generating the figure " ladders".
createFigure :: Integer -> [Point] -> [Point]
createFigure 0 !acc = Point 10 0 : acc
createFigure !size [] = createFigure size [Point 0 0]
createFigure !size (x : xs) =
  createFigure (size - 1) (plus x (addPoint size) : x : xs)
