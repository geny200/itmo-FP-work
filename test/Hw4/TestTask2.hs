{-# LANGUAGE Strict #-}

module Hw4.TestTask2
  ( -- * Function
    monteCarloTest,
  )
where

import Hw4.Task2 (parMonteCarlo)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (property)

rate :: (Floating a, Ord a) => a -> a -> Bool
rate was expected = (was - expected) * (was - expected) <= 0.1

-- | Unit tests for Task2.
monteCarloTest :: SpecWith ()
monteCarloTest =
  describe "Task2 - monteCarlo" $ do
    unitXYTest
    propCosTest
    propSinTest

-- | Unit tests for `MonteCarlo` integrate function.
unitXYTest :: SpecWith ()
unitXYTest = it "x = y | x = -y test" $ do
  rate (parMonteCarlo 1000 id (0, 1))  (0.5 :: Double) `shouldBe` True
  rate (parMonteCarlo 1000 negate (0, 1)) (-0.5 :: Double) `shouldBe` True
  rate (parMonteCarlo 1000 negate (-1, 1)) (0 :: Double) `shouldBe` True
  rate (parMonteCarlo 1000 id (-1, 1)) (0 :: Double) `shouldBe` True
  rate (parMonteCarlo 1000 sin (-1, 1)) (0 :: Double) `shouldBe` True
  rate (parMonteCarlo 1000 cos (0, pi)) (0 :: Double) `shouldBe` True

-- | Property test for `MonteCarlo` integrate function.
propCosTest :: SpecWith ()
propCosTest = it "x = cos(y), test" $ do
  property $ \x -> rate (parMonteCarlo 1000 cos (0.0, abs x)) ((sin . abs $ x) :: Double)

-- | Property test for `MonteCarlo` integrate function.
propSinTest :: SpecWith ()
propSinTest = it "x = sin(y), test" $ do
  property $ \x -> rate (parMonteCarlo 1000 sin (0.0, abs x)) ((1 -  (cos . abs $ x)) :: Double)
