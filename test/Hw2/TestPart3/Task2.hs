module Hw2.TestPart3.Task2
  ( -- * Functions
    testCombinators
  )
where

import Data.Char (isSpace)
import Data.Maybe (isNothing)
import Hw2.Part3.Task1 (runParser)
import Hw2.Part3.Task2 (element, eof, ok, satisfy, stream)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (property)

-- | Unit tests and Property-based tests for combinators
testCombinators :: SpecWith ()
testCombinators =
  describe "Part3.Task2 - combinators" $ do
    uTestOk
    uTestEof
    uTestSatisfy
    uTestElement
    uTestStream
    pTestOk
    pTestEof
    pTestSatisfyFail
    pTestSatisfyJust
    pTestElement
    pTestStream

-- | Unit tests for @ok@ combinator
uTestOk :: SpecWith ()
uTestOk = it "combinator ok" $ do
  runParser ok [4, 22, 27] `shouldBe` Just ((), [4, 22, 27] :: [Int])
  runParser ok [2.17, 3.1] `shouldBe` Just ((), [2.17, 3.1] :: [Rational])
  runParser ok "/n/t\n\t*" `shouldBe` Just ((), "/n/t\n\t*")
  runParser ok "sfsfsfsfs" `shouldBe` Just ((), "sfsfsfsfs")
  runParser ok "" `shouldBe` Just ((), "")

-- | Property-based tests for @ok@ combinator
pTestOk :: SpecWith ()
pTestOk = it "combinator ok str = Just" $ do
  property $ \str -> runParser ok (str :: String) == Just ((), str)

-- | Unit tests for @eof@ combinator
uTestEof :: SpecWith ()
uTestEof = it "combinator eof" $ do
  runParser eof "" `shouldBe` Just ((), "")
  runParser eof [] `shouldBe` Just ((), [] :: [Int])
  runParser eof [] `shouldBe` Just ((), [] :: [Maybe Bool])
  runParser eof "12341" `shouldBe` Nothing
  runParser eof "\n\t*" `shouldBe` Nothing
  runParser eof ([4, 22] :: [Int]) `shouldBe` Nothing

-- | Tester for @eof@ combinator
pEof :: String -> Bool
pEof [] = runParser eof ([] :: String) == Just ((), [])
pEof str = isNothing (runParser eof str)

-- | Property-based tests for @eof@ combinator
pTestEof :: SpecWith ()
pTestEof = it "combinator eof" $ do
  property pEof

-- | Unit tests for @satisfy@ combinator
uTestSatisfy :: SpecWith ()
uTestSatisfy = it "combinator satisfy" $ do
  runParser (satisfy isSpace) " hel" `shouldBe` Just (' ', "hel")
  runParser (satisfy isSpace) "\thel" `shouldBe` Just ('\t', "hel")
  runParser (satisfy isSpace) "hel" `shouldBe` Nothing
  runParser (satisfy (> 10)) [0 :: Int, 10, 20] `shouldBe` Nothing
  runParser (satisfy (< 10)) [0 :: Int, 10, 20] `shouldBe` Just (0, [10, 20])

-- | Property-based tests for @satisfy@ combinator
pTestSatisfyFail :: SpecWith ()
pTestSatisfyFail = it "combinator satisfy (/= x) (x : xs) = Nothing" $ do
  property $ \x xs -> 
    isNothing (runParser (satisfy (/= x)) ((x : xs) :: String))

-- | Property-based tests for @satisfy@ combinator
pTestSatisfyJust :: SpecWith ()
pTestSatisfyJust = it "combinator satisfy (== x) (x : xs) = Just" $ do
  property $ \x xs -> 
    runParser (satisfy (== x)) ((x : xs) :: String) == Just (x, xs)

-- | Unit tests for @element@ combinator
uTestElement :: SpecWith ()
uTestElement = it "combinator element" $ do
  runParser (element 'h') "hell" `shouldBe` Just ('h', "ell")
  runParser (element 'h') "just hell" `shouldBe` Nothing
  runParser (element 'j') "just hell" `shouldBe` Just ('j', "ust hell")
  runParser (element ' ') " hell" `shouldBe` Just (' ', "hell")
  runParser (element 1) [1, 2, 3] `shouldBe` Just (1 :: Int, [2, 3])
  runParser (element [1]) [[1], [2, 3]] `shouldBe` Just ([1 :: Int], [[2, 3]])

-- | Property-based tests for @element@ combinator
pTestElement :: SpecWith ()
pTestElement = it "combinator element y (x : xs)" $ do
  property $ \x xs y -> 
    (runParser (element y) ((x : xs) :: [Int]) == Just (x, xs)) == (y == x)

-- | Unit tests for @stream@ combinator
uTestStream :: SpecWith ()
uTestStream = it "combinator stream" $ do
  runParser (stream "hell") "hello" `shouldBe` Just ("hell", "o")
  runParser (stream "hello") "hillo" `shouldBe` Nothing
  runParser (stream "hello") "hell" `shouldBe` Nothing
  runParser (stream []) [4, 22, 27] `shouldBe` Just ([], [4, 22, 27] :: [Int])
  runParser (stream [1]) [1, 2, 3] `shouldBe` Just ([1] :: [Int], [2, 3])

-- | Property-based tests for @stream@ combinator
pTestStream :: SpecWith ()
pTestStream = it "combinator stream" $ do
  property $ \x xs -> 
    (runParser (stream x) (xs :: [Int]) == Just (x, drop (length x) xs)) == 
    (x == take (length x) xs)
