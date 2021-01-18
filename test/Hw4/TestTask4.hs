module Hw4.TestTask4
  ( -- * Function
    scriptTestRun
  )
where

import Hw4.HalyavaScript
  ( extractBool,
    extractDouble,
    extractInt,
    isEven,
    log2,
    perimeterCircle,
    sumTwoArg,
  )
import Hw4.Task4 (runHalyavaScript1, runHalyavaScript2)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (property)

-- | Unit and Property tests for @HalyavaScript@.
scriptTestRun :: SpecWith ()
scriptTestRun =
  describe "Task4 - run script" $ do
    it "Unit isEven" $ do
      show (runHalyavaScript1 isEven (10 :: Int)) `shouldBe` show True
      show (runHalyavaScript1 isEven (0 :: Int)) `shouldBe` show True
      show (runHalyavaScript1 isEven (15 :: Int)) `shouldBe` show False
      show (runHalyavaScript1 isEven True) `shouldBe` show False
      show (runHalyavaScript1 isEven False) `shouldBe` show True

    it "Unit log2" $ do
      show (runHalyavaScript1 log2 (17 :: Int)) `shouldBe` show (5 :: Int)
      show (runHalyavaScript1 log2 (16 :: Int)) `shouldBe` show (4 :: Int)
      show (runHalyavaScript1 log2 (2 :: Int)) `shouldBe` show (1 :: Int)
      show (runHalyavaScript1 log2 (4.5 :: Double)) `shouldBe` show (3 :: Int)

    it "Unit sumTwoArg" $ do
      show (runHalyavaScript2 sumTwoArg (17 :: Int) True) `shouldBe` show (18 :: Int)
      show (runHalyavaScript2 sumTwoArg True (4.5 :: Double)) `shouldBe` show (5.5 :: Double)
      show (runHalyavaScript2 sumTwoArg True True) `shouldBe` show (2 :: Int)
      show (runHalyavaScript2 sumTwoArg (4.5 :: Double) (4.5 :: Double)) `shouldBe` show (9 :: Double)
      show (runHalyavaScript2 sumTwoArg (10 :: Int) (4.5 :: Double)) `shouldBe` show (14.5 :: Double)

    it "Unit perimeterCircle" $ do
      show (runHalyavaScript1 perimeterCircle (15 :: Int)) `shouldBe` show (15 * 15 * pi :: Double)
      show (runHalyavaScript1 perimeterCircle (4.5 :: Double)) `shouldBe` show (4.5 * 4.5 * pi :: Double)
      show (runHalyavaScript1 perimeterCircle True) `shouldBe` show (pi :: Double)
      show (runHalyavaScript1 perimeterCircle (10 :: Int)) `shouldBe` show (100 * pi :: Double)

    it "isEven" $
      property $ \x ->
        extractBool (runHalyavaScript1 isEven (abs x :: Int))
          == even (abs x)
    it "isEven" $
      property $ \x ->
        extractBool (runHalyavaScript1 isEven (abs x :: Double))
          == even (ceiling (abs x) :: Int)

    it "log2" $
      property $ \x ->
        extractInt (runHalyavaScript1 log2 (abs x + 1 :: Int))
          == ceiling (logBase 2 (fromIntegral (abs x + 1)) :: Double)
    it "log2" $
      property $ \x ->
        extractInt (runHalyavaScript1 log2 (abs x + 1 :: Double))
          == ceiling (logBase 2 (fromIntegral (ceiling (abs x + 1) :: Int)) :: Double)

    it "sumTwoArg" $
      property $ \x y -> extractInt (runHalyavaScript2 sumTwoArg (x :: Int) (y :: Int)) == x + y
    it "sumTwoArg" $
      property $ \x y -> extractDouble (runHalyavaScript2 sumTwoArg (x :: Double) (y :: Double)) == x + y
    it "sumTwoArg" $
      property $ \x y -> extractDouble (runHalyavaScript2 sumTwoArg (x :: Double) (y :: Int)) == x + fromIntegral y
