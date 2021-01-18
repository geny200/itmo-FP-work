{-# LANGUAGE InstanceSigs #-}

module Hw2.TestPart3.Task4
  ( -- * Functions
    testListListParser
  )
where

import Hw2.Part3.Task4 (listlistParser)
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck.Property (property)
import Hw2.TestPart3.TestParser (testFail, testJustAll, testList, testListJust)

-- | Unit tests and Property-based tests for listlistParser
testListListParser :: SpecWith ()
testListListParser =
  describe "Part3.Task4 - listlistParser" $ do
    uTestListlistParser
    pTestListlistParser

-- | Creates a pair from the result of the string 
-- representation of the list and the list itself.
makeList 
  :: [[Int]]          -- ^ source list
  -> (String, [[Int]])-- ^ pair from the string represent of list and list
makeList xs = (show (ListList xs), xs)

-- | Unit tests for listlistParser
uTestListlistParser :: SpecWith ()
uTestListlistParser = it "parser listlistParser" $ do
  testListJust
    (testJustAll listlistParser)
    [ makeList [[1, 10], [5, -7, 2]],
      makeList [[0], [0], [0], [1, 1], [1, 1]],
      makeList [[], [], []]
    ]
  testList
    (testFail listlistParser)
    [ "1",
      "-1, 0, 2",
      "-3"
    ]

-- | Property-based tests for listlistParser
pTestListlistParser :: SpecWith ()
pTestListlistParser = it "parser listlistParser" $ do
  property $ \x xs -> testJustAll listlistParser (show (ListList (x : xs))) (x : xs)
  
-- | Auxiliary type for displaying the list in 
-- a string of the specified format
newtype ListList a 
  = ListList [[a]] -- ^ wrapper over the list

instance (Show a) => Show (ListList a) where
  show :: ListList a -> String
  show (ListList list) 
    = tail (foldr 
              (\y -> (++) 
                (foldl 
                    (\xs x -> xs ++ (" , " ++ show x))
                    (", " ++ (show . length $ y)) 
                y))
           "" list)
