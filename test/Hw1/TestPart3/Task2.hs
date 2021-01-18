module Hw1.TestPart3.Task2
  ( -- * Functions
    testSemigroupMonoid
  ) where

import Hw1.Common
import Hw1.Part3.Task2
import Test.HUnit

--------------------------
-- Unit tests
--------------------------

testSemigroupMonoid :: Test
testSemigroupMonoid = TestList
  [
    TestLabel "NonEmpty"   (testSemigroup
      dataElem dataElem dataElem),
      
    TestLabel "NonEmpty"   (testSemigroup
      (dataElem <> dataElem) dataElem dataElem),
      
    TestLabel "ThisOrThat" (testSemigroup
      (This "Hello") (That [1, 2]) (Both "World" [15, 65])),

    TestLabel "ThisOrThat" (testSemigroup
      (That [1, 2]) (Both "World" [15]) (This "Hell")),

    TestLabel "ThisOrThat" (testMonoid
      (Name "Hello") (Name "World! ") (Name "From Hell")),

    TestLabel "ThisOrThat" (testMonoid
      (Name "Hello") (Name "") (Name "From Hell")),
      
    TestCase (assertEqual "Endo" 
      (getEndo (Endo (*(-1)) <> (Endo (+7) <> Endo (*3))) 5)
      (getEndo ((Endo (*(-1)) <> Endo (+7)) <> Endo (*3)) 5)
      )
  ]

-----------------------------------------------
-- Secondary function for Unit tests
-----------------------------------------------
    
testSemigroup :: (Eq a, Show a, Semigroup a) => a -> a -> a -> Test
testSemigroup a b c = TestList
  [
    myTest "Semigroup <>" ((a <> b) <> c) (a <> (b <> c)),
    myTest "Semigroup <>" ((b <> c) <> a) (b <> (c <> a))
  ]
  
testMonoid :: (Eq a, Show a, Monoid a) => a -> a -> a -> Test
testMonoid a b c = TestList 
  [
    testSemigroup a b c,
    myTest "Monoid mempty" (mempty <> a) (a <> mempty),
    myTest "Monoid mempty" (mempty <> b) b,
    myTest "Monoid mempty" c (c <> mempty)
  ]

dataElem :: NonEmpty [Int]
dataElem = [1] :| [[0,2,0,3], [0,2,0,3]]