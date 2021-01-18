module Main where

import Hw3.ConsoleExplorer (consoleExplorer)
import Hw4.Task8 (evolve, gridWithOneIl)

main :: IO ()
main =
  do
    putStrLn "You can see examples in \'Main.hs\'."
    putStrLn "To run benchmarks and tests, use 'stack bench' and 'stack test'."

-- | Example how to start console explorer from `Hw3`.
runExplorer :: IO ()
runExplorer = consoleExplorer

-- | Example how to start game Covid-19 from `Hw4`.
runCovid19Game :: IO ()
runCovid19Game =
  do
    let steps = 10 :: Int
    putStrLn "start simulate Covid-19Game."
    simulate steps steps (gridWithOneIl 10 (2, 2, 2) (10, 10) (0.5 :: Float))
    putStrLn "end simulate Covid-19Game."
  where
    simulate _ 0 _ = putStrLn "End"
    simulate s x state =
      do
        putStrLn ("step " ++ show (s - x))
        print state
        simulate s (x - 1) (evolve state)
