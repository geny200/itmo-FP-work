module Hw1.TestPart2.Task1
  ( -- * Functions
    testFoldableThree
  ) where

import Hw1.Common
import Test.HUnit
import Data.List
import Hw1.Part1.Task3

--------------------------
-- Unit tests
--------------------------

-- | Unit tests for testing functions
-- foldr and foldMap (data `Three`)
testFoldableThree :: Test
testFoldableThree = TestList
  [
    TestLabel "foldr"   testFoldr,
    TestLabel "foldMap" testFoldMap
  ]

--------------------------
-- data data for testing
--------------------------

dataTestList :: [Int]
dataTestList = [1, 2, 8, 12, 9, 4, 1, 2, 5, 7, 0, -5, 8, 12]

dataTestThree :: Three Int
dataTestThree = fromList dataTestList

-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------

testFoldr :: Test
testFoldr = TestList
  [
    myTest "foldr Leaf" 0 (sum Leaf),
    myTest "foldr (+) 0" (sum dataTestList) (sum dataTestThree),
    myTest "foldr (-) 0" (foldr (-) 0 (sort dataTestList)) (foldr (-) 0 dataTestThree),
    myTest "foldr (*) 0" (product dataTestList) (product dataTestThree)
  ]

testFoldMap :: Test
testFoldMap = TestList
  [
    myTest "foldMap Leaf" 0 (sum Leaf),
    myTest "foldMap sum" (sum dataTestList) (sum dataTestThree),
    myTest "foldMap product" (product dataTestList) (product dataTestThree)
  ]