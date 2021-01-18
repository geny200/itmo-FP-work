{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hw4.Task4
  ( -- * Functions to run HalyavaScript
    runHalyavaScript0
  , runHalyavaScript1
  , runHalyavaScript2
  )
where

import Control.Applicative (liftA2)
import Control.Monad.ST (ST, runST)
import Data.Maybe (fromJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Typeable (cast, eqT)
import Hw4.HalyavaScript (CastValue (..), HalyavaScript (..), ValBox (..), box, isNil, nil)

-- | Constrain for a Num variable that can be cast.
type Value a = (Num a, CastValue a)

-- | Casts variables to the same type and performs 
-- an operation on them.
smartCast 
  :: forall a b. (CastValue a, CastValue b) 
  => (forall c. (CastValue c, Num c) => c -> c -> c) -- ^ Operation
  -> a                                               -- ^ First variable
  -> b                                               -- ^ Second variable
  -> ValBox                                          -- ^ Result of operation
smartCast f a b
  | (Just _) <- eqT @a @Bool,
    (Just _) <- eqT @b @Bool =
    box (f (toInt a) (toInt b))
  | (Just _) <- eqT @a @Double = box (f (fromJust (cast a)) (toDouble b))
  | (Just _) <- eqT @b @Double = box (f (toDouble a) (fromJust (cast b)))
  | (Just _) <- eqT @a @Int = box (f (fromJust (cast a)) (toInt b))
  | (Just _) <- eqT @b @Int = box (f (toInt a) (fromJust (cast b)))
  | otherwise = error "Unrecognized Type"

-- | Casts variables to the same type and performs 
-- an operation on them (returning a different type of result).
smartCastB 
  :: forall a b d. (CastValue a, CastValue b, CastValue d) 
  => (forall c. (CastValue c, Num c) => c -> c -> d) -- ^ Operation
  -> a                                               -- ^ First variable
  -> b                                               -- ^ Second variable
  -> ValBox                                          -- ^ Result of operation
smartCastB f a b
  | (Just _) <- eqT @a @Bool,
    (Just _) <- eqT @b @Bool =
    box (f (toInt a) (toInt b))
  | (Just _) <- eqT @a @Double = box (f (fromJust (cast a)) (toDouble b))
  | (Just _) <- eqT @b @Double = box (f (toDouble a) (fromJust (cast b)))
  | (Just _) <- eqT @a @Int = box (f (fromJust (cast a)) (toInt b))
  | (Just _) <- eqT @b @Int = box (f (toInt a) (fromJust (cast b)))
  | otherwise = error "Unrecognized Type"

-- | Performs an operation on existential types.
withCast 
  :: (forall c. (Value c) => c -> c -> c)            -- ^ Operation
  -> ValBox                                          -- ^ First variable
  -> ValBox                                          -- ^ Second variable
  -> ValBox
withCast f (MkBox a) (MkBox b) = 
  smartCast f a b

-- | Performs an operation on existential types.
withCastB 
  :: (CastValue d) 
  => (forall c. (Value c) => c -> c -> d)            -- ^ Operation
  -> ValBox                                          -- ^ First variable
  -> ValBox                                          -- ^ Second variable
  -> ValBox
withCastB f (MkBox a) (MkBox b) = 
  smartCastB f a b

-- | Implementation for executing a @HalyavaScript@.
instance HalyavaScript (ST s) where
  type Var (ST s) a = STRef s a
  value =
    return . box
  eRead ref =
    do
      var <- ref
      readSTRef var
  (@+@) = liftA2 (withCast (+))
  (@-@) = liftA2 (withCast (-))
  (@*@) = liftA2 (withCast (*))

  (@+) a b = a @+@ value b
  (@-) a b = a @-@ value b
  (@*) a b = a @*@ value b

  (@>) a b = a @>@ value b
  (@<) a b = a @<@ value b

  (@<@) = liftA2 (withCastB (<))
  (@>@) = liftA2 (withCastB (>))
  (@||@) = liftA2 (withCastB (\a b -> isNil a || isNil b))
  (@&&@) = liftA2 (withCastB (\a b -> isNil a && isNil b))
  (@==@) a b =
    do
      l <- a @<@ b
      r <- a @>@ b
      value . not $ (isNil l && isNil r)
  sNot a =
    do
      l <- a
      value . not $ isNil l
  (#) = (>>)
  sIf condition aThen aElse = do
    cond <- condition
    if isNil cond
      then aThen
      else aElse

  sWhile condition cycles =
    do
      cond <- condition
      if isNil cond
        then cycles # sWhile condition cycles
        else pure ()

  sFun1 fun arg =
    do
      res <- newSTRef . nil $ arg
      fun (return arg) $ return res
      readSTRef res

  sFun2 cont arg = sFun1 (cont (return arg))
  sWithVar arg fun =
    do
      ref <- newSTRef . box $ arg
      fun . return $ ref

  (@=) ref arg =
    ref @=@ value arg
  (@=@) ref arg =
    do
      v <- arg
      var <- ref
      writeSTRef var v

-- | Executes the @HalyavaScript@ and outputs a @ValueBox@
--  (which can be converted to any available view).
runHalyavaScript0 :: (forall s. ST s ValBox) -> ValBox
runHalyavaScript0 = runST

-- | Executes the @HalyavaScript@ with one argument and outputs
--  a @ValueBox@ (which can be converted to any available view).
runHalyavaScript1 
  :: (CastValue a) => 
  (forall s. ValBox -> ST s ValBox)               -- ^ Script
  -> a                                            -- ^ First argument
  -> ValBox                                       -- ^ Result
runHalyavaScript1 script arg = runST $ script . box $ arg

-- | Executes the @HalyavaScript@ with two arguments and outputs
--  a @ValueBox@ (which can be converted to any available view).
runHalyavaScript2 
  :: (CastValue a, CastValue b) 
  => (forall s. ValBox -> ValBox -> ST s ValBox)  -- ^ Script
  -> a                                            -- ^ First argument
  -> b                                            -- ^ Second argument
  -> ValBox                                       -- ^ Result
runHalyavaScript2 script arg1 = runHalyavaScript1 (script . box $ arg1)
