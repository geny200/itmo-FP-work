module Hw1.TestHw1
  ( -- * Function
    testHw1
  )
where

import Hw1.TestPart1.Task1 (testsWeeksDay)
import Hw1.TestPart1.Task2 (testNat, testPropNat)
import Hw1.TestPart1.Task3 (testThree)
import Hw1.TestPart2.Task1 (testFoldableThree)
import Hw1.TestPart2.Task2 (testPropSplitJoin, testSplitJoin)
import Hw1.TestPart3.Task1 (testConcat)
import Hw1.TestPart3.Task2 (testSemigroupMonoid)
import Test.HUnit

-- | Runs Unit tests and Property-based from the Hw1.
testHw1 :: IO ()
testHw1 =
  do
    putStrLn "HW1:"
    myPrint
      ( runTestTT
          ( TestList
              [ TestLabel "part-1-task-1" testsWeeksDay,
                TestLabel "part-1-task-2" testNat,
                TestLabel "part-1-task-3" testThree,
                TestLabel "part-2-task-1" testFoldableThree,
                TestLabel "part-2-task-2" testSplitJoin,
                TestLabel "part-3-task-1" testConcat,
                TestLabel "part-3-task-2" testSemigroupMonoid
              ]
          )
      )
    testPropNat
    testPropSplitJoin

myPrint :: IO Counts -> IO ()
myPrint input = input >>= print
