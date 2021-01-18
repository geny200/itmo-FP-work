module Hw2.TestPart2.Task1
  ( -- * Functions
    testExpr
  )
where

import Hw2.Part2.Task1 (ArithmeticError (..), Expr (..), eval)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (Property, property)

-- | Unit tests and Property-based tests for the @eval Expr@
testExpr :: SpecWith ()
testExpr =
  describe "Part2.Task1 - Expression" $ do
    uTestSumm
    uTestSub
    uTestMul
    uTestDiv
    uTestPow
    uTestExpr

-- | Sum tests
uTestSumm :: SpecWith ()
uTestSumm = uTestBinOp "summ" (:+) (+)

-- | Subtraction tests
uTestSub :: SpecWith ()
uTestSub = uTestBinOp "sub" (:-) (-)

-- | Multiplication tests
uTestMul :: SpecWith ()
uTestMul = uTestBinOp "multuply" (:*) (*)

-- | The division tests
uTestDiv :: SpecWith ()
uTestDiv = it "divide" $ do
  pTestBinOp
    (:/)
    ( \x y ->
        if y == 0
          then Left DivByZero
          else Right (x `div` y)
    )

-- | Exponentiation tests
uTestPow :: SpecWith ()
uTestPow = it "pow" $ do
  pTestBinOp
    (:^)
    ( \x y ->
        if y < 0
          then Left NegExp
          else Right (x ^ y)
    )

-- | Unit tests of expressions
uTestExpr :: SpecWith ()
uTestExpr = it "some expression" $ do
  eval (Const 4 :+ Const 22 :* Const 27 :/ Const 58)
    `shouldBe` Right (4 + 22 * 27 `div` 58)

  eval (Const 58 :+ Const 27 :* Const 22 :/ Const 4)
    `shouldBe` Right (58 + 27 * 22 `div` 4)

  eval (Const 58 :+ Const 27 :* Const 22 :^ Const 4)
    `shouldBe` Right (58 + 27 * 22 ^ 4)

type BinOp a
  = a -> a -> a -- ^ binary operation

-- | A function that creates named tests for a binary function
uTestBinOp
  :: String     -- ^ name of tests
  -> BinOp Expr -- ^ function under test
  -> BinOp Int  -- ^ function that gives the expected result for first function
  -> SpecWith ()-- ^ named tests for binary function
uTestBinOp name exprOp intOp = it name $ do
  pTestBinOp exprOp (rightBinOp intOp)

-- | Wraps the result of executing the function in `Right`
rightBinOp
  :: BinOp Int                                  -- ^ function to be wrapped
  -> Int                                        -- ^ the first argument
  -> Int                                        -- ^ second argument
  -> Either ArithmeticError Int                 -- ^ result in `Right`
rightBinOp op x y = Right (x `op` y)

-- | A function that tests a binary function
pTestBinOp ::
  BinOp Expr ->                                 -- ^ function under test
  (Int -> Int -> Either ArithmeticError Int) -> -- ^ gives the expected result
  Property                                      -- ^ tests for binary function
pTestBinOp exprOp intOp = property $ \x y ->
  eval (Const x `exprOp` Const y) `shouldBe` (x `intOp` y)
