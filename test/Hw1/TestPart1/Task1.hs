module Hw1.TestPart1.Task1 
  ( -- * Functions
    testsWeeksDay
  ) where

import Hw1.Common
import Hw1.Part1.Task1
import Test.HUnit

--------------------------
-- Unit tests
--------------------------
-- | Unit tests for `WeeksDay` 
-- test for functions: nextDay, daysToParty, isWeekend, afterDays
testsWeeksDay :: Test
testsWeeksDay = TestList 
  [
    TestLabel "daysToParty" testDaysToParty, 
    TestLabel "nextDay"     testNextDay,
    TestLabel "isWeekend"   testIsWeekend,
    TestLabel "afterDays"   testAfterDays
  ]

--------------------------
-- data for testing
--------------------------

dataListNums :: [Int]
dataListNums = [0..6]

-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------

testDaysToParty :: Test
testDaysToParty 
  = myTestMap "daysToParty" (6 -) (fromEnum . daysToParty . toEnum) dataListNums
    
testIsWeekend :: Test
testIsWeekend 
  = myTestMap "isWeekend" ( < 2) (isWeekend . toEnum) dataListNums

testNextDay :: Test
testNextDay 
  = myTestMap 
    "daysToParty" 
    (\n -> (1 + n) `mod` 7) 
    (fromEnum . nextDay . toEnum) 
    [0..6]
    
testAfterDays :: Test
testAfterDays 
  = myTestMap 
     "daysToParty" 
     (\(x, y) -> (x + y) `mod` 7) 
     (\(x, y) -> fromEnum (afterDays (toEnum x) y )) 
     [(x, y) | x <- dataListNums, y <- dataListNums]