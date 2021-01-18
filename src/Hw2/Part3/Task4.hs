module Hw2.Part3.Task4
  ( -- * Combinators
    listlistParser
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Hw2.Part3.Task1 (Parser (..))
import Hw2.Part3.Task2 (satisfy)
import Hw2.Part3.Task3 (readInt, readNatural)

-- | The parser for the list of lists
-- of comma separated numbers.
listlistParser :: Parser Char [[Int]]
listlistParser =
  space >> listParser
    >>= (\x -> (x :) <$> (comma >> listlistParser) <|> pure [x])

-- | Reads the number of items in the list,
-- and then reads the list
listParser :: Parser Char [Int]
listParser = space >> readNatural >>= parseList

-- | Parser for a list of @n@ `Int`
-- separated by a comma.
parseList
  :: Int                -- ^ the number of elements in the list
  -> Parser Char [Int]  -- ^ list parser
parseList 0 = pure []
parseList n =
  comma >> readInt
    >>= (\x -> (x :) <$> parseList (n - 1))

-- | A parser that consumes any number
-- of whitespace characters.
space :: Parser Char ()
space =
  (satisfy isSpace >> space)
    <|> pure ()

-- | A parser that consumes any number
-- of whitespace characters before and
-- after the comma, along with the comma.
comma :: Parser Char ()
comma =
       space
    >> satisfy (== ',')
    >> space
