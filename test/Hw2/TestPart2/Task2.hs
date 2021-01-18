module Hw2.TestPart2.Task2
  ( -- * Functions
    testMoving
  )
where

import Hw2.Part2.Task2 (moving)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests for the moving average function with a description
testMoving :: SpecWith ()
testMoving = describe "Part2.Task2 - avg moving" uTestMoving

-- | Unit tests for the moving average function
uTestMoving :: SpecWith ()
uTestMoving = it "moving" $ do
  moving 4 [1, 5, 3, 8, 7, 9, 6]
    `shouldBe` ([1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5] :: [Double])

  moving 2 [1, 5, 3, 8, 7, 9, 6]
    `shouldBe` ([1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5] :: [Rational])

  moving 5 [1, 5, 3, 8, 7, 9, 6]
    `shouldBe` ([1.0, 3.0, 3.0, 4.25, 4.8, 6.4, 6.6] :: [Rational])

  moving 4 [1, 4 .. 20]
    `shouldBe` ([1.0, 2.5, 4.0, 5.5, 8.5, 11.5, 14.5] :: [Rational])

  moving 2 [20, 14 .. -20]
    `shouldBe` ([20.0, 17.0, 11.0, 5.0, -1.0, -7.0, -13.0, -19.0] :: [Rational])
