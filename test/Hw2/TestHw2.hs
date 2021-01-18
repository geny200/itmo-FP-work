module Hw2.TestHw2
  ( -- * Function
    testHw2,
  )
where

import Hw2.TestPart1.Task1 (testStringSum)
import Hw2.TestPart2.Task1 (testExpr)
import Hw2.TestPart2.Task2 (testMoving)
import Hw2.TestPart3.Task2 (testCombinators)
import Hw2.TestPart3.Task3 (testSimpleParser)
import Hw2.TestPart3.Task4 (testListListParser)
import Test.Hspec (SpecWith, describe)

-- | Runs Unit and Prop-base tests for Hw2.
testHw2 :: SpecWith ()
testHw2 =
  describe "HW2:" $
    do
      testStringSum
      testExpr
      testMoving
      testCombinators
      testSimpleParser
      testListListParser
