{-# LANGUAGE InstanceSigs #-}

module Hw2.Part1.Task3
  ( -- * `NonEmpty` constructors
    NonEmpty(..)
  )
where

import Control.Applicative (liftA2)
import Data.Foldable (fold)

data NonEmpty a
  = a :| [a]        -- ^ constructor with two elements type a and list a

-- | Helper function, creates a list from an `NonEmpty`.
toList
  :: NonEmpty a     -- ^ `NonEmpty` that needs to be converted into a list
  -> [a]            -- ^ list retrieved from the `NonEmpty`
toList (x :| xs) = x : xs

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs
  
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (l :| ls) <*> (x :| xs) = l x :| (ls <*> xs)

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x :| xs) >>= f
    = let r :| rs = f x
       in r :| fold (rs : fmap (toList . f) xs)

instance Foldable NonEmpty where
  foldMap :: (Monoid m) => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = mappend (f x) (foldMap f xs)

instance Traversable NonEmpty where
  sequenceA :: Applicative f => NonEmpty (f a) -> f (NonEmpty a)
  sequenceA (x :| xs) = liftA2 (:|) x (sequenceA xs)