module Hw1.Part2.Task2
  ( -- * Functions
    joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

-- | Split by specified character.
splitOn
  :: (Eq a)
  => a              -- ^ special character
  -> [a]            -- ^ [@a@] to split
  -> NonEmpty [a]   -- ^ `NonEmpty` [@a@] with sublist
splitOn x = foldr (split x) ([] :| [])

-- | Concatenates lists with a special character.
joinWith 
  :: a              -- ^ special character
  -> NonEmpty [a]   -- ^ `NonEmpty` [@a@] with lists
  -> [a]            -- ^ concatenated list
joinWith x = foldl1 (join x)

------------------------
-- Secondary functions
------------------------

join :: a -> [a] -> [a] -> [a]
join x line = (++) (line ++ [x])

split :: (Eq a) => a -> a -> NonEmpty [a] -> NonEmpty [a]
split x element (sHead :| sTail)
  | x == element  = [] :| (sHead : sTail)
  | otherwise     = (element : sHead) :| sTail