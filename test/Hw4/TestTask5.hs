module Hw4.TestTask5
  ( -- * Function
    scriptTestShow
  )
where

import Hw4.HalyavaScript (isEven, log2, perimeterCircle, sumTwoArg)
import Hw4.Task5 (showHalyavaScript1, showHalyavaScript2)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests for @HalyavaScript@.
scriptTestShow :: SpecWith ()
scriptTestShow =
  describe "Task5 - show script" $ do
    it "isEven" $
      showHalyavaScript1 isEven `shouldBe` isEvenStr

    it "log2" $
      showHalyavaScript1 log2 `shouldBe` log2Str

    it "perimeterCircle" $
      showHalyavaScript1 perimeterCircle `shouldBe` perimeterCircleStr

    it "sumTwoArg" $
      showHalyavaScript2 sumTwoArg `shouldBe` sumTwoArgStr

sumTwoArgStr :: String
sumTwoArgStr =
  "function(arg0, arg1) {\n\
  \\tvar v0;\n\
  \\tv0 = arg0 + arg1;\n\
  \\treturn v0;\n\
  \}"

perimeterCircleStr :: String
perimeterCircleStr =
  "function(arg1) {\n\
  \\tvar v0;\n\
  \\tv0 = 3.141592653589793;\n\
  \\tv0 = v0 * arg1 * arg1;\n\
  \\treturn v0;\n\
  \}"

log2Str :: String
log2Str =
  "function(arg1) {\n\
  \\tvar v0;\n\
  \\tvar v1 = 0;\n\
  \\tv1 = 1;\n\
  \\tv0 = 0;\n\
  \\twhile (arg1 > v1) {\n\
  \\t\tv1 = v1 + v1;\n\
  \\t\tv0 = v0 + 1;\n\
  \\t}\n\
  \\treturn v0;\n\
  \}"

isEvenStr :: String
isEvenStr =
  "function(arg1) {\n\
  \\tvar v0;\n\
  \\tvar v1 = 0;\n\
  \\tv0 = True;\n\
  \\twhile (arg1 > v1) {\n\
  \\t\tv0 = !v0;\n\
  \\t\tv1 = v1 + 1;\n\
  \\t}\n\
  \\treturn v0;\n\
  \}"
