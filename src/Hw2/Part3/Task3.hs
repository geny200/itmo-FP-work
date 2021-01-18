module Hw2.Part3.Task3
  ( -- * Combinators
    rbs
  , readInt
  , readNatural
  )
where

import Control.Applicative (some, (<|>))
import Data.Char (isDigit)
import Hw2.Part3.Task1 (Parser (..))
import Hw2.Part3.Task2 (element, eof, satisfy)

-- | A parser of the correct bracket sequence
-- that takes up the entire stream (ends with eof).
rbs :: Parser Char ()
rbs = rbs' >> eof

-- | Parser of the correct bracket sequence.
rbs' :: Parser Char ()
rbs' = (element '(' >> rbs' >> element ')' >> rbs') <|> pure ()

-- | An `Int` parser that can be preceded by a + or - sign.
readInt :: Parser Char Int
readInt =
  (       (element '+' >> pure id)
      <|> (element '-' >> pure negate)
      <|> pure id
  )
    <*> readNatural

-- | `Int` parser
readNatural :: Parser Char Int
readNatural = read <$> some (satisfy isDigit) 
