{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hw4.Task5
  ( -- * Function to show HalyavaScript
    showHalyavaScript0
  , showHalyavaScript1
  , showHalyavaScript2
  )
where

import Control.Monad.State (State, evalState, get, mapState, put, runState)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Hw4.HalyavaScript
  ( CastValue (..),
    HalyavaScript (..),
    NValue (..),
    ValBox (..),
    box,
  )

-- | Newtype for the text representation of the script
newtype Name = Name String deriving (Eq, Ord)

instance Show Name where
  show (Name a) = a

instance NValue Name where
  isNil (Name a) = null a
  nil = const . Name $ []

instance CastValue Name where
  toInt (Name a) = length a
  toDouble (Name a) = fromIntegral . length $ a

-- | Creates a text representation of the operation
-- based on two variables and the operation name.
commonWrite
  :: (Monad m, Show a)
  => String           -- ^ Operation name
  -> m ValBox         -- ^ Left variable
  -> a                -- ^ Right variable
  -> m ValBox
commonWrite name a b =
  do
    l <- a
    return . box . Name $ (show l ++ name ++ show b)

-- | Creates a text representation of the operation
-- based on two variables and the operation name.
commonWrite2
  :: Monad m =>
  String              -- ^ Operation name
  -> m ValBox         -- ^ Left variable
  -> m ValBox         -- ^ Right variable
  -> m ValBox
commonWrite2 name a b =
  do
    l <- a
    r <- b
    return . box . Name $ (show l ++ name ++ show r)

-- | Creates a text representation of a function
-- from the arguments and body of the function.
makeFunction
  :: String           -- ^ Body of function
  -> [ValBox]         -- ^ Arguments of functions
  -> String           -- ^ Value name
  -> String
makeFunction body args val =
  "function("
    ++ intercalate ", " (fmap show args)
    ++ ") {\n"
    ++ "\tvar "
    ++ val
    ++ ";\n\t"
    ++ body
    ++ "\n\treturn "
    ++ val
    ++ ";\n}"

-- | Adds a tab to the code.
addTab
  :: String           -- ^ The code in which you want to add the tabs
  -> String           -- ^ The same code, but with tabs
addTab = intercalate "\n\t" . splitOn "\n"

-- | Implementation for converting a @HalyavaScript@ to code.
instance HalyavaScript (State String) where
  type Var (State String) a = String
  value = return . box . Name . show
  eRead = mapState $ first (box . Name)

  (@+@) = commonWrite2 " + "
  (@-@) = commonWrite2 " - "
  (@*@) = commonWrite2 " * "

  (@+) = commonWrite " + "
  (@-) = commonWrite " - "
  (@*) = commonWrite " * "

  (@>) = commonWrite " > "
  (@<) = commonWrite " < "

  (@<@) = commonWrite2 " < "
  (@>@) = commonWrite2 " > "

  (@||@) = commonWrite2 " || "
  (@&&@) = commonWrite2 " && "
  (@==@) = commonWrite2 " == "
  sNot a =
    do
      val <- a
      return . box . Name $ ("!" ++ show val)

  (#) f1 f2 =
    do
      num <- get
      let cnt = read num :: Int
          (_, firste) = runState f1 (show (cnt + 1))
          (_, second) = runState f2 (show (cnt + 2))
      put (firste ++ "\n" ++ second)
      pure ()

  sIf condition aThen aElse = do
    num <- get
    cond <- condition
    let (_, sThen) = runState aThen num
        (_, sElse) = runState aElse num
    put
      ( "if (" ++ show cond ++ ") {\n\t" ++ addTab sThen
          ++ "\n} else {\n\t"
          ++ addTab sElse
          ++ "\n}"
      )
    pure ()

  sWhile condition cycles =
    do
      num <- get
      cond <- condition
      let (_, s) = runState cycles num
      put
        ( "while (" ++ show cond
            ++ ") {\n\t"
            ++ addTab s
            ++ "\n}"
        )
      pure ()

  sFun1 fun arg =
    do
      let (_, s) = runState (fun (return arg) (return "v0")) "0"
      return (box . Name $ makeFunction (addTab s) [arg] "v0")

  sFun2 fun arg1 arg2 =
    do
      let (_, s) = runState (fun (return arg1) (return arg2) (return "v0")) "0"
      return (box . Name $ makeFunction (addTab s) [arg1, arg2] "v0")

  sWithVar arg fun =
    do
      num <- get
      let cnt = read num :: Int
          var = "v" ++ show (cnt + 1)
          (_, s) = runState (fun (return var)) (show (cnt + 2))
      put ("var " ++ var ++ " = " ++ show arg ++ ";\n" ++ s)
      pure ()

  (@=) ref arg =
    ref @=@ value arg

  (@=@) ref arg =
    do
      right <- arg
      left <- ref
      put (left ++ " = " ++ show right ++ ";")
      pure ()

-- | Converts @HalyavaScript@ to code.
showHalyavaScript0 :: State String ValBox -> String
showHalyavaScript0 script = show (evalState script "main\n")

-- | Converts @HalyavaScript@ with one argument to code.
showHalyavaScript1 :: (ValBox -> State String ValBox) -> String
showHalyavaScript1 script = show (evalState (script . box . Name $ "arg1") "")

-- | Converts @HalyavaScript@ with two arguments to code.
showHalyavaScript2 :: (ValBox -> ValBox -> State String ValBox) -> String
showHalyavaScript2 script = showHalyavaScript1 (script . box . Name $ "arg0")
