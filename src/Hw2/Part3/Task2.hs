module Hw2.Part3.Task2
  ( -- * Combinators
    element
  , eof
  , ok 
  , satisfy
  , stream
  )
where

import Hw2.Part3.Task1 (Parser (..))

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
  :: (s -> Bool)    -- ^ predicate
  -> Parser s s     -- ^ the parser for predicate
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
  => s            -- ^ specified element
  -> Parser s s   -- ^ the parser for that element
element x = satisfy (== x)

-- | Creates a parser that parses several specified
-- stream elements.
stream
  :: (Eq s)
  => [s]          -- ^ the specified elements
  -> Parser s [s] -- ^ the parser for that elements
stream = foldr (\x -> (<*>) ((:) <$> element x)) (pure [])
