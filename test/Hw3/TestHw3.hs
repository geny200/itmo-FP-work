module Hw3.TestHw3
  ( -- * Function
    testHw3
  )
where

import Hw3.SimpleTest (testFileSys)
import Test.Hspec (SpecWith, describe)

-- | Function for running tests for Hw3.
testHw3 :: SpecWith ()
testHw3 =
  describe "HW3:" testFileSys
