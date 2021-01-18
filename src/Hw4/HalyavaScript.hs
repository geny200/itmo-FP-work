{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hw4.HalyavaScript
  ( -- * HalyavaScript
    HalyavaScript (..)

    -- * ValBox constructors
  , ValBox (..)

    -- * NValue constructors
  , NValue (..)

    -- * CastValue constructors
  , CastValue (..)

    -- Function
  , box

    -- * Functions to extract value
  , extractBool
  , extractInt
  , extractDouble

    -- * Functions to demonstrate
  , log2
  , sumTwoArg
  , isEven
  , perimeterCircle
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)

-- | Existential type for saving variables
data ValBox = forall a. CastValue a => MkBox a

-- | Function for creating a ValBox
-- (like @MkBox@)
box :: CastValue t => t -> ValBox
box = MkBox

-- | Functions to extract @Bool@ from ValBox
extractBool :: ValBox -> Bool
extractBool = isNil

-- | Functions to extract @Int@ from ValBox
extractInt :: ValBox -> Int
extractInt (MkBox a) = toInt a

-- | Functions to extract @Double@ from ValBox
extractDouble :: ValBox -> Double
extractDouble (MkBox a) = toDouble a

instance Show ValBox where
  show (MkBox a) = show a

class (Show a, Typeable a) => NValue a where
  isNil :: a -> Bool
  nil :: a -> a

class (Ord a, NValue a) => CastValue a where
  toInt :: a -> Int
  toDouble :: a -> Double

instance NValue Int where
  isNil = (/= 0)
  nil = const 0

instance NValue Bool where
  isNil = (/= False)
  nil = const False

instance NValue Double where
  isNil = (/= 0)
  nil = const 0

instance NValue ValBox where
  isNil (MkBox a) = isNil a
  nil (MkBox a) = MkBox (nil a)

instance CastValue Bool where
  toInt = fromEnum
  toDouble = fromIntegral . toInt

instance CastValue Int where
  toInt = id
  toDouble = fromIntegral

instance CastValue Double where
  toInt = fromEnum
  toDouble = id

type BinaryOp m a b c = m a -> m b -> m c

type BinOp m = BinaryOp m ValBox ValBox ValBox

type BinaryOpL m a b c = m a -> b -> m c

type BinOpL m a = BinaryOpL m ValBox a ValBox

type Condition m a = BinaryOp m a () ()

type Func0 m v = (m v -> m ())

type Func1 m v = (m ValBox -> Func0 m v)

type Func2 m v = (m ValBox -> Func1 m v)

infixl 7 @*@

infixl 6 @+@, @-@

infix 4 @<@, @>@, @==@

infixr 3 @&&@

infixr 2 @||@

infix 1 @=@

infixl 0 #

-- | Declaration of @HalyavaScript@
class HalyavaScript js where
  type Var js a :: Type
  value :: (CastValue a) => a -> js ValBox
  eRead :: js (Var js ValBox) -> js ValBox
  (@+@) :: BinOp js
  (@-@) :: BinOp js
  (@*@) :: BinOp js

  (@+) :: (CastValue a) => BinOpL js a
  (@-) :: (CastValue a) => BinOpL js a
  (@*) :: (CastValue a) => BinOpL js a

  (@>) :: (CastValue a) => BinOpL js a
  (@<) :: (CastValue a) => BinOpL js a

  (@<@) :: BinOp js
  (@>@) :: BinOp js
  (@||@) :: BinOp js
  (@&&@) :: BinOp js
  (@==@) :: BinOp js
  sNot :: js ValBox -> js ValBox

  (#) :: BinaryOp js () () ()
  sIf :: js ValBox -> Condition js ()
  sWhile :: Condition js ValBox
  sFun1 :: Func1 js (Var js ValBox) -> ValBox -> js ValBox
  sFun2 :: Func2 js (Var js ValBox) -> ValBox -> ValBox -> js ValBox
  sWithVar :: (CastValue a) => a -> Func0 js (Var js ValBox) -> js ()

  (@=) :: (CastValue a) => js (Var js ValBox) -> a -> js ()
  (@=@) :: js (Var js ValBox) -> js ValBox -> js ()

-- | For a given @x@ calculates @ceiling (log2 x)@
log2 :: (HalyavaScript js) => ValBox -> js ValBox
log2 =
  sFun1 $ \a logCnt ->
    sWithVar (0 :: Int) $ \accum ->
      accum @= (1 :: Int) # 
      logCnt @= (0 :: Int) # 
      sWhile (a @>@ eRead accum) ( 
        (accum @=@ eRead accum @+@ eRead accum)# 
        (logCnt @=@ eRead logCnt @+ (1 :: Int))
      )

-- | For a given @x@ calculates @even x@
isEven :: (HalyavaScript js) => ValBox -> js ValBox
isEven =
  sFun1 $ \a evenRes ->
    sWithVar (0 :: Int) $ \accum ->
      evenRes @= True #
      sWhile (a @>@ eRead accum) ( 
        evenRes @=@ sNot (eRead evenRes) # 
        accum @=@ eRead accum @+ (1 :: Int)
      )

-- | For a given @a@ and @b@ calculates @a + b@
sumTwoArg :: (HalyavaScript js) => ValBox -> ValBox -> js ValBox
sumTwoArg = sFun2 $ \a b res -> res @=@ a @+@ b

-- | For a given @r@ calculates @pi * r * r@
perimeterCircle :: (HalyavaScript js) => ValBox -> js ValBox
perimeterCircle = 
  sFun1 $ \r res ->
    res @= (pi :: Double) # 
    res @=@ eRead res @*@ r @*@ r
