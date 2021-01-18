{-# LANGUAGE InstanceSigs #-}

module Hw1.Part1.Task2
  ( -- * `Nat` constructor
    Nat(..)
    
    -- * Functions
  , evenF
  , oddF
  ) where

data Nat 
  = Z     -- ^ Zero
  | S Nat -- ^ Recursive definition `Nat`
  
-- | Returns @True@ if the `Nat` is even, @False@ otherwise.
evenF 
  :: Nat  -- ^ given `Nat`
  -> Bool -- ^ @True@ if the `Nat` is even
evenF Z = True
evenF (S (S x)) = evenF x
evenF _ = False

-- | Returns @True@ if the `Nat` is odd, @False@ otherwise.
oddF 
  :: Nat  -- ^ given `Nat`
  -> Bool -- ^ @True@ if the `Nat` is odd
oddF = not . evenF 

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  x + Z = x
  Z + x = x
  x + (S y) = S (x + y)

  (-) :: Nat -> Nat -> Nat
  Z - _ = Z
  x - Z = x
  (S x) - (S y) = x - y

  (*) :: Nat -> Nat -> Nat
  _ * Z = Z
  Z * _ = Z
  x * (S Z) = x
  x * (S y) = x * y + x

  fromInteger :: Integer -> Nat
  fromInteger x
    | x > 0     = S (fromInteger (x - 1))
    | otherwise = Z

  abs :: Nat -> Nat
  abs = id

  signum :: Nat -> Nat
  signum _ = S Z

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z = True
  _ == Z = False
  Z == _ = False
  (S x) == (S y) = x == y

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  Z <= _ = True
  (S _) <= Z = False
  (S x) <= (S y) = x <= y

instance Real Nat where
  toRational :: Nat -> Rational
  toRational Z = 0
  toRational (S x) = 1 + toRational x

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum = fromInteger . toInteger

  fromEnum :: Nat -> Int
  fromEnum Z = 0
  fromEnum (S x) = 1 + fromEnum x

instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger Z = 0
  toInteger (S x) = 1 + toInteger x

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem Z _ = (Z, Z)
  quotRem _ Z = (Z, Z)
  quotRem x y
    | x < y     = (Z, x)
    | x == y    = (S Z, Z)
    | otherwise = let (x1, y1) = quotRem (x - y) y
                    in (x1 + S Z, y1)