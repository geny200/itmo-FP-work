module Hw4.TestHw4
  ( -- * Function
    testHw4
  )
where

import Hw4.TestTask1 (geometryTest)
import Hw4.TestTask2 (monteCarloTest)
import Hw4.TestTask3 (chtTest)
import Hw4.TestTask4 (scriptTestRun)
import Hw4.TestTask5 (scriptTestShow)
import Hw4.TestTask6 (lensTest)
import Hw4.TestTask7 (fsTest)
import Test.Hspec (SpecWith, describe, hspec)

-- | Function for running all tests
testHw4 :: SpecWith ()
testHw4 =
  describe "HW4:" $ do
    geometryTest
    monteCarloTest
    chtTest
    scriptTestRun
    scriptTestShow
    lensTest
    fsTest
