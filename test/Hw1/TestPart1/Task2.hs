module Hw1.TestPart1.Task2
  ( -- * Functions
    testNat
  , testPropNat
  ) where

import Hw1.Common
import Hw1.Part1.Task2 (Nat(..), evenF, oddF)
import Test.QuickCheck
import Test.HUnit

--------------------------
-- Unit tests
--------------------------

-- | Unit tests for `Nat` 
-- test for operations: *, +, -, div, mod, ==, <=, toInteger, fromInteger
testNat :: Test
testNat = TestList (
  map testBinaryOp dataListOfTestsInt 
  ++ map testBinaryOp dataListOfTestsBool)

--------------------------
-- Property-based
--------------------------

-- | Property-based tests for `Nat`
-- test for operations: *, +, -, div, mod, ==, <=, toInteger, fromInteger
testPropNat :: IO ()
testPropNat 
  = foldl1 mappend 
    (map myQuickCheck dataListOfTestsInt 
      ++ map myQuickCheck dataListOfTestsBool)
      
--------------------------
-- data data for testing
--------------------------

-- | Test function definitions
dataListOfTestsInt :: 
  [( String                         -- ^ function name
   , Integer -> Integer -> Integer  -- ^ function returns the expected value
   , Nat -> Nat -> Integer          -- ^ function returns the actual value
  )]
dataListOfTestsInt = 
  [
    ("multyply", (*),             operationNatInteger (*)),
    ("sub",      absNeg,          operationNatInteger (-)),
    ("summ",     (+),             operationNatInteger (+)),
    ("div",      divByZeroOp div, divByZeroNatOp . operationNatInteger $ div),
    ("mod",      divByZeroOp mod, divByZeroNatOp . operationNatInteger $ mod),
    ("toInteger . fromInteger",   const,  operationNatInteger const)
  ]

-- | Test function definitions
dataListOfTestsBool :: 
  [( String                         -- ^ function name
   , Integer -> Integer -> Bool     -- ^ function returns the expected value
   , Nat -> Nat -> Bool             -- ^ function returns the actual value
  )]
dataListOfTestsBool = 
  [
    ("eq",   (==),       (==)), 
    ("ord",  (<=),       (<=)),
    ("odd",  const.odd,  const.oddF),
    ("even", const.even, const.evenF)
  ]
  
-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------
  
divByZeroOp :: (Integer -> Integer -> a) -> (Integer -> Integer -> a)
divByZeroOp op x y 
  | y == 0    = op x (y + 1)
  | otherwise = op x y   
  
divByZeroNatOp :: (Nat -> Nat -> a) -> (Nat -> Nat -> a)
divByZeroNatOp op x Z = op x (S Z)
divByZeroNatOp op x y = op x y  

absNeg :: Integer -> Integer -> Integer
absNeg x y 
  | x > y     = x - y
  | otherwise = 0 

makeBinaryListTest :: (Integer -> Integer -> Test) -> Test
makeBinaryListTest op = TestList [op x y | x <- [0 .. 11], y <- [0 .. 11]]

operationNat :: (Nat -> Nat -> a) -> Integer -> Integer -> a
operationNat op x y = op (fromInteger x) (fromInteger y)

operationNatInteger :: (Nat -> Nat -> Nat) -> Nat -> Nat -> Integer
operationNatInteger op x y = toInteger $ op x y

operation :: 
  (Eq a, Show a) => 
  String -> 
  (Integer -> Integer -> a) -> 
  (Integer -> Integer -> a) -> 
  Integer -> 
  Integer -> 
  Test
operation name opInt opNat x y 
  = myTest name (opInt x y) (opNat x y)

testBinaryOp :: 
  (Eq a, Show a) => 
  (String, Integer -> Integer -> a, Nat -> Nat -> a) 
  ->  Test
testBinaryOp (name, opInt, opNat) 
  = makeBinaryListTest (operation name opInt (operationNat opNat))

-----------------------------------------------
-- Secondary function for Property-based tests
-----------------------------------------------

operationEq ::
  (Eq a) =>
  (Integer -> Integer -> a) ->
  (Integer -> Integer -> a) ->
  Integer ->
  Integer ->
  Bool
operationEq opInt opNat x y
  = opInt x y == opNat x y
  
absOp :: (Integer -> Integer -> a) -> (Integer -> Integer -> a)
absOp op x y = op (abs x) (abs y)  

myQuickCheck :: (Eq a) => (String, Integer -> Integer -> a, Nat -> Nat -> a) -> IO ()
myQuickCheck (name, opInt, opNat) 
  = do 
      print name
      quickCheck (
        operationEq 
        (absOp opInt) 
        (absOp . operationNat $ opNat))
