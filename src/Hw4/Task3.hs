{-# LANGUAGE ExistentialQuantification #-}

module Hw4.Task3
  ( -- * ConcurrentHashTable data type
    ConcurrentHashTable

    -- * Functions
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  )
where

import Control.Concurrent.STM.TVar
  ( TVar,
    modifyTVar,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Monad ((>=>))
import Control.Monad.STM (STM, atomically)
import Data.Hashable (Hashable, hash)
import Data.Vector (Vector, forM_, generateM, (!))
import Lens.Micro ((<&>))

type Element k v = TVar (Maybe (k, v))

type HashVector k v = Vector (Element k v)

-- | Date type for a parallel hash table.
data ConcurrentHashTable k v
  = (Hashable k, Ord k)
  => ConcurrentHashTable (TVar (HashVector k v)) (TVar Int)

-- | Creates a hash table in @IO@.
newCHT :: (Hashable k, Ord k) => IO (ConcurrentHashTable k v)
newCHT =
  do
    vector <- generateM initialLength (const emptyElement)
    tVarLength <- newTVarIO 0
    tVarVector <- newTVarIO vector
    return (ConcurrentHashTable tVarVector tVarLength)

-- | Returns a @Just@ value in @IO@ from the hash table
-- by key if it exists, @Nothing@ otherwise.
getCHT
  :: k                        -- ^ Search key
  -> ConcurrentHashTable k v  -- ^ Hash table
  -> IO (Maybe v)
getCHT =
  modifyCHT id (readTVar >=> (\y -> return (y <&> snd)))

-- | Inserts a value by key into the hash table.
putCHT
  :: k                        -- ^ Search key
  -> v                        -- ^ Inserted value
  -> ConcurrentHashTable k v  -- ^ Hash table
  -> IO ()
putCHT key value =
  modifyCHT succ (\e -> writeTVar e (Just (key, value))) key

-- | Returns the number of inserted elements in the hash table.
sizeCHT
  :: ConcurrentHashTable k v  -- ^ Hash table
  -> IO Int                   -- ^ Number of elements in the hash table
sizeCHT (ConcurrentHashTable _ tVarLength) =
  readTVarIO tVarLength


-------------------------------------------
--         Auxiliary functions           --
-------------------------------------------

-- -- | Initialized initial size of the hash table
initialLength :: Int
initialLength = 10

-- | Default initialized element in the hash table
emptyElement :: IO (Element k v)
emptyElement = newTVarIO Nothing

-- | Returns a hash table Element by key
-- in the @STM@ monad.
find
  :: (Hashable k, Ord k)
  => k                        -- ^ Search key
  -> HashVector k v           -- ^ Hash array
  -> STM (Element k v)
find key vector =
  bloodhound (hash key)
  where
    bloodhound position =
      let index = position `mod` length vector
          variable = vector ! index
       in do
            value <- readTVar variable
            case value of
              Nothing -> return variable
              Just (k, _) ->
                if k == key
                  then return variable
                  else bloodhound (succ position)

-- | Creates a new hash array from the old one.
-- If the fullness value is less than the allowed
-- value, no rehashing occurs.
rehash
  :: (Hashable k, Ord k)
  => Double                   -- ^ Fullness value
  -> HashVector k v           -- ^ Hash array
  -> STM (HashVector k v)
rehash trigger vector
  | trigger < 0.7 = return vector
  | otherwise =
    do
      newVector <- generateM (length vector * 3) (const (newTVar Nothing))
      forM_ vector $ \e ->
        do
          value <- readTVar e
          case value of
            Nothing -> pure ()
            Just val ->
              ( do
                  variable <- find (fst val) newVector
                  writeTVar variable value
              )
      return newVector

-- | Universal operation with a hash table.
-- Makes a modification with the found element by key.
modifyCHT
  :: (Int -> Int)             -- ^ Size Counter
  -> (Element k v -> STM a)   -- ^ Modification of the element
  -> k                        -- ^ Search key
  -> ConcurrentHashTable k v  -- ^ Hash table
  -> IO a
modifyCHT cnt action key (ConcurrentHashTable vector size) =
  atomically $
    do
      unVector <- readTVar vector
      unSize <- readTVar size
      workVector <-
        rehash
          (fromIntegral unSize / (fromIntegral . length $ unVector))
          unVector
      writeTVar vector workVector
      variable <- find key workVector
      value <- readTVar variable
      cntAction value variable
  where
    cntAction Nothing var = do modifyTVar size cnt; action var
    cntAction (Just _) var = action var
