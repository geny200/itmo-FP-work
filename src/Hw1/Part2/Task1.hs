{-# LANGUAGE InstanceSigs #-}

module Hw1.Part2.Task1
  ( -- * Three constructors
    Three(..)
    
    -- * Converts to the list
  , toList
  ) where

import Data.Foldable  (toList)
import Hw1.Part1.Task3    (Three(..))

-- | customize 'Foldable' instance
instance Foldable Three where
  foldr :: (a -> b -> b) -> b -> Three a -> b
  foldr _ iv Leaf = iv
  foldr f iv (Node left list right) 
    = foldr f (foldr f (foldr f iv right) list) left

  foldMap :: Monoid m => (a -> m) -> Three a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node left list right) =
    mappend (foldr (mappend . f) (foldMap f left) list) (foldMap f right)