module Hw1.TestPart3.Task1
  ( -- * Functions
    testConcat
  ) where

import Hw1.Common
import Hw1.Part3.Task1
import Test.HUnit
import Data.Maybe
import Data.Either

--------------------------
-- Unit tests
--------------------------

-- | Unit tests for testing functions
-- maybeConcat and eitherConcat
testConcat :: Test
testConcat = TestList
  [
    TestLabel "maybeConcat Char"  (testMaybeConcat dataTestString),
    TestLabel "maybeConcat Int"   (testMaybeConcat dataTestInt),
    TestLabel "eitherConcat"      (testEitherConcat dataTestEither)
  ]

--------------------------
-- data for testing
--------------------------

dataTestInt :: [[Maybe [Int]]]
dataTestInt = 
  [
    [Just [1,2,3], Nothing, Just [4,5]],
    [Just [1,2,3], Just [4,5], Just [4,5]],
    [Nothing, Nothing, Nothing],
    []
  ]
  
dataTestString :: [[Maybe String]]
dataTestString = 
  [
    [Just "Hell", Nothing, Just " World"],
    [Just "Hell", Just "o", Just " World"],
    [Nothing, Nothing, Nothing],
    []
  ]
  
dataTestEither :: [[Either String [Int]]]
dataTestEither = 
  [
    [Left "Hell", Right [1,2,3], Left " World", Right [4,5]],
    [Right [1,2,3], Right [4,5]],
    [Left "Hell", Left " World"],
    []
  ]
  
-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------
  
concatTuple :: 
  (Monoid a, Monoid b) 
  => ([a], [b]) 
  -> (a, b)
concatTuple (left, right) 
  = (foldl mappend mempty left, foldl mappend mempty right)

testMaybeConcat ::
  (Eq a, Show a) 
  => [[Maybe [a]]] 
  -> Test
testMaybeConcat = 
  myTestMap "maybeConcat" (concat . catMaybes) maybeConcat
 
testEitherConcat ::
  (Eq a, Eq b, Show a, Show b, Monoid a, Monoid b) 
  => [[Either a b]] 
  -> Test
testEitherConcat 
  = myTestMap "eitherConcat" (concatTuple . partitionEithers) eitherConcat