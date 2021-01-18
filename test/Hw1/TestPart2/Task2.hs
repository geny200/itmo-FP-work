module Hw1.TestPart2.Task2
  ( -- * Functions
    testSplitJoin
  , testPropSplitJoin
  ) where

import Hw1.Common
import Test.HUnit
import Data.Foldable
import Test.QuickCheck
import Hw1.Part2.Task2      as Task
import Data.List.Split  as Split
import Data.List.NonEmpty (NonEmpty)

--------------------------
-- Unit tests
--------------------------

-- | Unit tests for testing functions
-- splitOn and JoinWith
testSplitJoin :: Test
testSplitJoin = TestList
  [
    TestLabel "splitOn /"   (testSplitOn '/'),
    TestLabel "splitOn ' '" (testSplitOn ' '),
    TestLabel "joinWith / . splitOn /" (testJoinWith '/'),
    TestLabel "joinWith _ . splitOn _" (testJoinWith ' ')
  ]
  
--------------------------
-- Property-based
--------------------------

-- | Property-based tests for testing functions
-- splitOn and JoinWith
testPropSplitJoin :: IO ()
testPropSplitJoin
  = do
      putStrLn "splitOn; joinWith . splitOn"
      quickCheck (testList Split.splitOn Task.splitOn Task.joinWith)
      
--------------------------
-- data for testing
--------------------------

-- | Unit test definitions
dataTestString :: [String]
dataTestString = 
  [
    "hello world",
    "/ test/ TestPart2 /Task2.hs/",
    "test// /TestPart2/Task2. hs ",
    ""
  ]

-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------

testSplitOn ::Char -> Test
testSplitOn x = 
  myTestMap 
    "splitOn" 
    (Split.splitOn [x]) 
    (toList . Task.splitOn x) 
    dataTestString
 
testJoinWith ::Char -> Test
testJoinWith x = 
  myTestMap 
    "joinWith x . splitOn x" 
    id 
    (Task.joinWith x . Task.splitOn x) 
    dataTestString

-----------------------------------------------
-- Secondary function for Property-based tests
-----------------------------------------------

testList
  :: ([Char] -> [Char] -> [[Char]])
  -> (Char -> [Char] -> NonEmpty [Char])
  -> (Char -> NonEmpty [Char] -> [Char])
  -> Char
  -> [Char]
  -> Bool
testList splitOne splitTwo join x list
  = splitOne [x] list == (toList . splitTwo x) list
  && (list == (join x . splitTwo x) list)