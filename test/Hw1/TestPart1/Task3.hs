module Hw1.TestPart1.Task3
  ( -- * Functions
    testThree
  ) where

import Hw1.Common
import Hw1.Part1.Task3 
import Hw1.Part2.Task1
import Test.HUnit
import Data.List

--------------------------
-- Unit tests
--------------------------

-- | Unit tests for `Three`
-- test for functions: isEmpty, lengthF, findF, remove, insertF, fromList
testThree :: Test
testThree = TestList
  [
    TestLabel "isEmpty"    testEmpty,
    TestLabel "lengthF"    testLength,
    TestLabel "findF"      testFindF,
    TestLabel "remove"     testRemove,
    TestLabel "insertF"    testInsertF,
    TestLabel "fromList"   testFromList
  ]

--------------------------
-- data data for testing
--------------------------

dataTestList :: [Int]
dataTestList 
  = [1, 2, 8, 12, 9, 4, 1, 2, 5, 7, 0, -5, 8, 12]

dataTestThree :: Three Int
dataTestThree = fromList dataTestList

testEmpty :: Test
testEmpty = TestList
  [
    myTest "isEmpty Leaf" True (isEmpty Leaf),
    myTest "isEmpty" False (isEmpty (Node Leaf [True] Leaf)),
    myTest "isEmpty" False (isEmpty dataTestThree)
  ]

-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------

testLength :: Test
testLength = myTest "lengthF" 14 (lengthF dataTestThree)

testFindF :: Test
testFindF = TestList
  [
    myTest "findF Leaf" False (findF Leaf True),
    myTest "findF" True  (findF dataTestThree 1),
    myTest "findF" True  (findF dataTestThree 12),
    myTest "findF" False (findF dataTestThree 3),
    myTest "findF" False (findF dataTestThree 6)
  ]

testInsertF :: Test
testInsertF =
  let editableThree = insertF (insertF dataTestThree 2) 6
  in TestList
  [
    myTest "lengthF . insertF" 16    (lengthF editableThree),
    myTest "findF . insertF"   False (findF editableThree 3),
    myTest "findF . insertF"   True  (findF editableThree 6)
  ]

testRemove :: Test
testRemove =
  let editableThree = remove (remove dataTestThree 9) 6
  in TestList
  [
    myTest "lengthF . remove" 13    (lengthF editableThree),
    myTest "findF . remove"   False (findF editableThree 9),
    myTest "findF . remove"   False (findF editableThree 6)
  ]

testFromList :: Test
testFromList 
  = myTest 
    "toList . fromList == sort" 
    True 
    ((toList . fromList $ dataTestList) == sort dataTestList)