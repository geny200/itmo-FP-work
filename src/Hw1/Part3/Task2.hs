{-# LANGUAGE InstanceSigs #-}

module Hw1.Part3.Task2
  (-- * Endo constructor
    Endo(..)

   -- * NonEmpty constructor
  , NonEmpty(..)

   -- * Name constructor
  , Name(..)

   -- * ThisOrThat constructor
  , ThisOrThat(..)
) where

data NonEmpty a = 
  a :| [a]        -- ^ constructor with two elements type a and list a
  deriving ( Eq   -- ^ default 'Eq' instance
           , Show -- ^ default 'Show' instance
           )

instance (Semigroup a) => Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (l :| x) <> (r :| y) = (l <> r) :| (x <> y)

data ThisOrThat a b
  = This a        -- ^ constructor this - type a variant
  | That b        -- ^ constructor that - type b variant
  | Both a b      -- ^ constructor both - both type variant
  deriving ( Eq   -- ^ default 'Eq' instance
           , Show -- ^ default 'Show' instance
           )

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (This x)   <> (That y)   = Both x y
  (That x)   <> (This y)   = Both y x
  (This x)   <> (This y)   = This (x <> y)
  (That x)   <> (That y)   = That (x <> y)
  (That x)   <> (Both l r) = Both l (x <> r)
  (This x)   <> (Both l r) = Both (x <> l) r
  (Both l r) <> (That x)   = Both l (r <> x)
  (Both l r) <> (This x)   = Both (l <> x) r
  (Both l r) <> (Both x y) = Both (l <> x) (r <> y)

newtype Name = Name
  { unName :: String -- ^ Name
  }
  deriving ( Eq      -- ^ default 'Eq' instance
           , Show    -- ^ default 'Show' instance
           )

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (Name l) <> (Name []) = Name l
  (Name []) <> (Name r) = Name r
  (Name l) <> (Name r) = Name (l <> "." <> r)

instance Monoid Name where
  mempty :: Name
  mempty = Name []

newtype Endo a = Endo
  { getEndo :: a -> a -- ^ Function get endo
  }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (Endo l) <> (Endo r) = Endo (r . l)
  
instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id