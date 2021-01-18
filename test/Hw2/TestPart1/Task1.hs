module Hw2.TestPart1.Task1
  ( -- * Functions
    testStringSum,
  )
where

import Data.Foldable (foldr')
import Hw2.Part1.Task1 (stringSum)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (property)

-- | Unit tests and Property-based tests for the @stringSum@
testStringSum :: SpecWith ()
testStringSum = describe "Part1.Task1 - StringSum" $ do
  unitJustTest
  pTestStream

-- | Data for unit testing
testData :: [[Int]]
testData =
  [ [1 .. 20],
    [1, 3 .. 20],
    [1, 6 .. 50],
    [20, 19 .. -20],
    [100, 90 .. 0],
    [20, 15 .. -100]
  ]

-- | Auxiliary function for translating a list from a string
listToString :: [Int] -> String
listToString = foldr' (\x str -> show x ++ " " ++ str) ""

-- | Tester for @stringSum@
uTest :: [Int] -> IO ()
uTest list =
  (stringSum . listToString $ list) `shouldBe` (Just . sum $ list)

-- | Unit tests for @stringSum@
unitJustTest :: SpecWith ()
unitJustTest = it "unit StringSum" $ do
  foldr ((>>) . uTest) (pure ()) testData

-- | Property-based tests for @stringSum@
pTestStream :: SpecWith ()
pTestStream = it "prop StringSum" $ do
  property $ \xs -> (stringSum . listToString $ xs) == (Just . sum $ xs)
