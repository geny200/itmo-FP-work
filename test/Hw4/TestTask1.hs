module Hw4.TestTask1
  ( -- * Function
    geometryTest
  )
where

import Hw4.Task1 (Point (..), doubleArea, perimeter)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests for Task1.
geometryTest :: SpecWith ()
geometryTest =
  describe "Task1 - geometry" $ do
    unitPerimeterTest
    unitAreaTest

-- | Unit tests for `perimeter` function.
unitPerimeterTest :: SpecWith ()
unitPerimeterTest = it "perimeter test" $ do
  perimeter rectangeOne `shouldBe` 6
  perimeter triangleOne `shouldBe` 12
  
-- | Unit tests for `doubleArea` function.
unitAreaTest :: SpecWith ()
unitAreaTest = it "area test" $ do
  doubleArea rectangeOne `shouldBe` 4
  doubleArea triangleOne `shouldBe` 12

rectangeOne :: [Point]
rectangeOne = [Point 0 0, Point 2 0, Point 2 1, Point 0 1]

triangleOne :: [Point]
triangleOne = [Point 0 0, Point 3 0, Point 0 4]