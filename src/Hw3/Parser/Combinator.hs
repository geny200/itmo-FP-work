module Hw3.Parser.Combinator
  ( -- * Combinators
    element
  , eof
  , greedily
  , ok
  , satisfy
  , space
  , stream
  , wordText
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Hw3.Parser.Parser (Parser (..))

-- | The parser never crashes or consumes input
ok :: Parser s ()
ok = pure ()

-- | Checks that the parser has reached the end
-- of the data stream (otherwise it fail).
eof :: Parser s ()
eof = Parser f
  where
    f [] = Just ((), [])
    f _ = Nothing

-- | The parser accepts a predicate on a stream
-- element, and returns the element, absorbing
-- it from the stream, if the predicate on the
-- element is `True`, otherwise it falls.
satisfy
  :: (s -> Bool)      -- ^ predicate
  -> Parser s s       -- ^ the parser for predicate
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

-- | Creates a parser that parses the specified
-- single element of the stream.
element
  :: (Eq s)
  => s              -- ^ specified element
  -> Parser s s     -- ^ the parser for that element
element x = satisfy (== x)

-- | Creates a parser that parses several specified
-- stream elements.
stream
  :: (Eq s)
  => [s]            -- ^ the specified elements
  -> Parser s [s]   -- ^ the parser for that elements
stream = foldr (\x -> (<*>) ((:) <$> element x)) (pure [])

-- | A parser that consumes any number
-- of whitespace characters.
space :: Parser Char ()
space =
  (satisfy isSpace >> space)
    <|> pure ()

-- | A parser that consumes any number
-- of characters which satisfy the predicate.
allWhile 
  :: (s -> Bool)    -- ^ the predicate
  -> Parser s [s]   -- ^ the parser for that predicate stream
allWhile p = (:) <$> satisfy p <*> (allWhile p <|> pure [])

-- | A parser that consumes correct file 
-- name string
wordText :: Parser Char String
wordText = 
  space >> 
    (     (element '\"' *> allWhile(/= '\"') <* element '\"') 
      <|> allWhile (not . isSpace)
    )

-- | The parser consumes all occurrences turning them 
-- into an array
arrayJoin 
  :: (a -> a -> a)  -- ^ combinator
  -> Parser s a     -- ^ occurrence Parser
  -> a              -- ^ default value
  -> Parser s a     -- ^ the resulting array parser 
arrayJoin combine pars el = 
      (combine <$> pars <*> arrayJoin combine pars el) 
  <|> pure el

-- | The parser greedily consumes the maximum 
-- possible number of occurrences turning it into an array
greedily :: Parser s (a -> a) -> Parser s (a -> a)
greedily x = arrayJoin (.) x id
