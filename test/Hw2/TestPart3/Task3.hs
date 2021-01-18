module Hw2.TestPart3.Task3
  ( -- * Functions
    testSimpleParser
  )
where

import Hw2.Part3.Task3 (rbs, readInt)
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck.Property (property)
import Hw2.TestPart3.TestParser (testFail, testJustAll, testList, testListJust)

-- | Unit tests and Property-based tests for correct
-- bracket sequence and readInt
testSimpleParser :: SpecWith ()
testSimpleParser =
  describe "Part3.Task3 - simple parsers" $ do
    uTestRbs
    uTestInt
    pTestInt

-- | Unit tests for correct bracket sequence
uTestRbs :: SpecWith ()
uTestRbs = it "parser rbs" $ do
  testList
    testRbsJust
    [ "",
      "()()()",
      "(()())",
      "(()(()))",
      "(()())()"
    ]
  testList
    (testFail rbs)
    [ "())",
      "(())()))",
      "()_()",
      "()(()",
      "())(()",
      "()Hello",
      "Hello",
      "Hello()",
      "(Hell)"
    ]

-- | Helping function that starts the parser
-- for the correct bracket sequence.
testRbsJust :: String -> IO ()
testRbsJust str = testJustAll rbs str ()

-- | Unit tests for readInt
uTestInt :: SpecWith ()
uTestInt = it "parser readInteger" $ do
  testListJust
    (testJustAll readInt)
    [ ("10", 10),
      ("-100", -100),
      ("0000", 0),
      ("-000", 0),
      ("+000", 0),
      ("-1242", -1242),
      ("+1242", 1242),
      ("1242", 1242)
    ]
  testList
    (testFail readInt)
    [ " 1242",
      "x0F",
      "-",
      "+",
      ""
    ]

-- | Property-based tests for readInt
pTestInt :: SpecWith ()
pTestInt = it "parser readInt" $ do
  property $ \x -> testJustAll readInt (show x) x
