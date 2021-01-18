{-# LANGUAGE Strict #-}

module Hw4.TestTask3
  ( -- * Function
    chtTest
  )
where

import Control.Concurrent.Async (mapConcurrently, withAsync, wait, cancel)
import Control.Monad (guard)
import Data.Maybe (isJust)
import Hw4.Task3 (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck.Property (property)
import Control.Exception.Base (SomeException, catch)

-- | Unit and property tests for @ConcurrentHashTable@.
chtTest :: SpecWith ()
chtTest =
  describe "Task3 - CHT" $ do
    unitFunctionalTest
    unitLinearizationTest
    unitErrorTest

-- | Linearization Unit tests for @ConcurrentHashTable@.
unitLinearizationTest :: SpecWith ()
unitLinearizationTest = it "linearization test" $ do
  cht <- newCHT
  resList <- mapConcurrently (linearizationWorkerUniq (>) cht) (take 1000 [1 ..])
  filter isJust resList `shouldBe` []

-- | Functional Property tests for @ConcurrentHashTable@.
unitFunctionalTest :: SpecWith ()
unitFunctionalTest = it "functional test" $ do
  property $ \xs ->
    do
      cht <- newCHT
      resList <-
        mapM
          (linearizationWorkerUniq (\a b -> a == succ b) cht)
          (take 1000 xs)
      guard . not $ any isJust resList
      
-- | Functional Property tests for @ConcurrentHashTable@.
unitErrorTest :: SpecWith ()
unitErrorTest = it "async exception test" $ do
  cht <- newCHT
  resList <- mapConcurrently (exceptionWorkerUniq (>) cht) (take 1000 [1 ..])
  filter isJust resList `shouldBe` []
      
-------------------------------------------
--         Auxiliary functions           --
-------------------------------------------

-- | Generates an error message if the expected results do 
-- not match the expected results.
errorMessage 
  :: Int                -- ^ Expected number
  -> Maybe Int          -- ^ Actual number 
  -> Int                -- ^ Actual size
  -> (Int -> Bool)      -- ^ The predicate for the actual size
  -> Maybe String 
errorMessage expected Nothing _ _ =
  Just ("expected num: " ++ show expected ++ ", but was: Nothing")
errorMessage expected (Just was) size predicate
  | expected /= was =
    Just
      ( "expected num: "
          ++ show expected
          ++ ", but was: "
          ++ show was
      )
  | predicate size =
    Just
      ( "unexpected size: "
          ++ show size
      )
  | otherwise = Nothing

-- | A function to execute in a separate thread tests 
-- linearizability of @ConcurrentHashTable@.
linearizationWorkerUniq 
  :: (Int -> Int -> Bool)         -- ^ Predicate to estimate the actual 
                                  --   size of the expected.
  -> ConcurrentHashTable Int Int  -- ^ @ConcurrentHashTable@ under test
  -> Int                          -- ^ Number for put and then get from table
  -> IO (Maybe String)
linearizationWorkerUniq predicate table num =
  do
    initialSize <- sizeCHT table
    putCHT num num table
    resSize <- sizeCHT table
    resVal <- getCHT num table
    return (errorMessage num resVal resSize (predicate initialSize))
    
-- | The function catches all exceptions.
catchAll ::
  IO a ->                       -- ^ input `IO`
  (SomeException -> IO a) ->    -- ^ Error handler
  IO a                          -- ^ output `IO`
catchAll = catch

-- | A function to execute in a separate thread async exception
--  tests for @ConcurrentHashTable@.
exceptionWorkerUniq 
  :: (Int -> Int -> Bool)         -- ^ Predicate to estimate the actual 
                                  --   size of the expected.
  -> ConcurrentHashTable Int Int  -- ^ @ConcurrentHashTable@ under test
  -> Int                          -- ^ Number for put and then get from table
  -> IO (Maybe String)
exceptionWorkerUniq predicate table num =
  do 
    withAsync (linearizationWorkerUniq predicate table num) $ \a1 -> do
      withAsync (linearizationWorkerUniq predicate table num) $ \a2 -> do
         cancel a1
         _ <- catchAll (wait a1) (const . return $ Nothing)
         wait a2
