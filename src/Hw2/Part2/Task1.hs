module Hw2.Part2.Task1
  ( -- * `Expr` constructors
    Expr (..)

    -- * `ArithmeticError` constructors
  , ArithmeticError(..)

    -- * Function
  , eval
  )
where

data Expr
  = Const Int
  | Expr :+ Expr  -- ^ the sum of two expressions
  | Expr :- Expr  -- ^ subtracting the second expression from the first
  | Expr :* Expr  -- ^ multiplying expressions
  | Expr :/ Expr  -- ^ dividing expressions
  | Expr :^ Expr  -- ^ raising the first expression to a power

infixl 6 :+

infixl 6 :-

infixl 7 :*

infixl 7 :/

infixr 8 :^

data ArithmeticError
  = DivByZero   -- ^ division by zero error
  | NegExp      -- ^ error raising to a negative power
  deriving (Show, Eq)

type BinOp a 
  = a -> a -> a -- ^ binary operation

-- | Helper function for calculating the operation value.
-- Calculates the result for a binary operation.
binaryOp :: BinOp Int -> Expr -> Expr -> Either ArithmeticError Int
binaryOp op a b =
  do
    x <- eval a
    y <- eval b
    return (x `op` y)

-- | Evaluates the result of the expression if possible and
-- return `Right` `Int`, otherwise returns an error in the
-- `Left` `ArithmeticError`
eval
  :: Expr                       -- ^ the expression to calculate
  -> Either ArithmeticError Int -- ^ the result of the calculation
eval (Const a) = return a
eval (a :+ b) = binaryOp (+) a b
eval (a :- b) = binaryOp (-) a b
eval (a :* b) = binaryOp (*) a b
eval (a :/ b) =
  do
    x <- eval a
    y <- eval b
    if y == 0
      then Left DivByZero
      else return (x `div` y)
eval (a :^ b) =
  do
    x <- eval a
    y <- eval b
    if y < 0
      then Left NegExp
      else return (x ^ y)
