{-# LANGUAGE InstanceSigs #-}

module Hw2.Part1.Task2
  ( -- * `Tree` constructors
    Tree(..)
  )
where

import Control.Applicative (liftA2)

data Tree a
  = Branch (Tree a) (Tree a)  -- ^ Node with two with two kids `Tree`
  | Leaf a                    -- ^ Leaf with value
  
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b 
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  
instance Applicative Tree where
  pure :: a -> Tree a 
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f) <*> (Leaf x) = Leaf (f  x)
  f <*> (Branch l r) = Branch (f <*> l) (f <*> r)
  (Branch fl fr) <*> x = Branch (fl <*> x) (fr <*> x)
    
instance Monad Tree where
  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (Leaf x) >>= f = f x
  (Branch l r) >>= f =  Branch (l >>= f) (r >>= f)
  
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (Leaf x) = f x
  foldMap f (Branch l r) = mappend (foldMap f l) (foldMap f r)

instance Traversable Tree where 
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA (Leaf x) = Leaf <$> x
  sequenceA (Branch l r) = liftA2 Branch (sequenceA l) (sequenceA r)