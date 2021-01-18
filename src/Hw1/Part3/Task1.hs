module Hw1.Part3.Task1
  ( -- * Functions
    eitherConcat
  , maybeConcat
  ) where

-- | Accepts a list of Maybe lists and concatenates them
maybeConcat 
  :: [Maybe [a]]          -- ^ [@`Maybe` [a]@] a list of Maybe lists
  -> [a]                  -- ^ concatenated list
maybeConcat = foldl _concat []

-- | Accepts a list of Either and concatenates them to tuple
eitherConcat 
  :: (Monoid a, Monoid b) 
  => [Either a b]         -- ^ list of Either
  -> (a, b)               -- ^ tuple of concatenated list
eitherConcat = foldl _either (mempty, mempty)

------------------------
-- Secondary functions
------------------------

_concat :: [a] -> Maybe [a] -> [a]
_concat list Nothing = list
_concat list (Just x) = list ++ x

_either :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
_either (left, right) (Left x) = (mappend left x, right)
_either (left, right) (Right x) = (left, mappend right x)