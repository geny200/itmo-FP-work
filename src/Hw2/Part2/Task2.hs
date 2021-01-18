module Hw2.Part2.Task2
  ( -- * Function
    moving
  )
where

import Control.Monad.Trans.State (evalState, get, modify)

-- | Calculates the moving average for all input data.
moving 
  :: Fractional a 
  => Int      -- ^ how many elements to count the average
  -> [a]      -- ^ source data to calculate the moving average for
  -> [a]      -- ^ average values were calculated
moving x list = evalState (movingAvg list) []
  where
    movingAvg [] = return []
    movingAvg (y : ys) =
      do
        modify ((y :) . take (x - 1))
        cur <- get
        let value = sum cur / fromIntegral (length cur)
        end <- movingAvg ys
        return (value : end)
