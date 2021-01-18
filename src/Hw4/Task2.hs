{-# LANGUAGE Strict #-}

module Hw4.Task2
  ( -- * Functions
    lineMonteCarlo
  , parMonteCarlo
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Par (InclusiveRange (..), parMapReduceRange, runPar)

-- | Performs calculations for a single point.
itemMonteCarlo :: (Floating a) => (a -> a) -> a -> a -> Int -> a 
itemMonteCarlo f from delta index = f (from + delta * fromIntegral index)

-- | Linear realization of the MonteCarlo method.
lineMonteCarlo 
  :: (Floating a) 
  => Int                        -- ^ Number of points
  -> (a -> a)                   -- ^ Function for integration
  -> (a, a)                     -- ^ Area in which function will be integrated.
  -> a                          -- ^ The result of integration
lineMonteCarlo n f (from, to) =
  let delta = (to - from) / fromIntegral n
   in delta * sum (map (itemMonteCarlo f from delta) [1 .. n])

-- | Parallel realization of the MonteCarlo method.
parMonteCarlo 
  :: (Floating a, NFData a) 
  => Int                        -- ^ Number of points
  -> (a -> a)                   -- ^ Function for integration
  -> (a, a)                     -- ^ Area in which function will be integrated.
  -> a                          -- ^ The result of integration
parMonteCarlo n f (from, to) = 
  let delta = (to - from) / fromIntegral n
      range = InclusiveRange 1 n
      mapper x = return (itemMonteCarlo f from delta x)
      reducer x y = return (x + y)
   in delta * runPar (parMapReduceRange range mapper reducer 0)
