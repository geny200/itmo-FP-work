{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Hw4.Task6
  ( -- * Constructors FS
    FS(..)

    -- * Lenses
  , contents
  , name

    -- * Prisms
  , _Dir
  , _File

    -- * Functions
  , readFSTree
  
   -- * Functions for test lens
  , addSuffixRoot
  , changeRootName
  , directoryName
  , fileName
  , getFileNames
  , getFirstSubDirName
  , getSubDirs
  )
where

import Lens.Micro (Lens', Traversal, each, lens, (%~), (&), (.~), (<&>), (^.), (^..), (^?), _Just)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeFileName, (</>))

data FS
  = Dir
      {
        _name :: FilePath,    -- ^ Folder name, not full path
        _contents :: [FS]     -- ^ List of directory contents
      }
  | File
      {
        _name :: FilePath     -- ^ File name, not full path
      }
  deriving (Show, Eq)

-----------------------------------------------
--                Base Lens                  --
-----------------------------------------------

name :: Lens' FS FilePath
name = lens _name (\fs v -> fs {_name = v})

contents :: Lens' FS [FS]
contents = lens content directoryLens
  where
    directoryLens :: FS -> [FS] -> FS
    directoryLens (File n) _ = File n
    directoryLens (Dir n _) v = Dir n v
    content (File _) = []
    content (Dir _ v) = v  

-----------------------------------------------
--                Base Prisms                --
-----------------------------------------------

_File :: Traversal FS FS FS FS
_File f (File a) = f (File a)
_File _ (Dir a b) = pure (Dir a b)

_Dir :: Traversal FS FS FS FS
_Dir _ (File a) = pure (File a)
_Dir f (Dir a b) = f (Dir a b)

-----------------------------------------------
--    Recursively parse the File system      --
-----------------------------------------------

-- | Returns the FS like file system view object 
-- from given directory
readFSTree :: FilePath -> IO FS
readFSTree path =
  do
    names <- listDirectory path
    values <- sequenceA (names <&> (path </>) <&> recursiveReadFS)
    return $ Dir (takeBaseName path) (values ^.. each . _Just)

recursiveReadFS :: FilePath -> IO (Maybe FS)
recursiveReadFS path =
  do
    isDir <- doesDirectoryExist path
    isFil <- doesFileExist path
    recursiveFS isFil isDir
  where
    recursiveFS False False = return Nothing
    recursiveFS True _ = return . return . File . takeFileName $ path
    recursiveFS _ True =
      do
        names <- listDirectory path
        values <- sequenceA (names <&> (path </>) <&> recursiveReadFS)
        return . return $ Dir (takeBaseName path) (values ^.. each . _Just)

-----------------------------------------------
-------- Block - Практика на линзы   ----------
-----------------------------------------------

getSubDirs :: FS -> [FS]
getSubDirs fs = fs ^. _Dir . contents ^.. each . _Dir

directoryName :: FS -> Maybe String
directoryName fs = fs ^? _Dir . name

fileName :: FS -> String
fileName fs = fs ^. _File . name

changeRootName :: FS -> FS
changeRootName fs = fs & _Dir . name .~ "/"

addSuffixRoot :: FS -> String -> FS
addSuffixRoot fs suffix = fs & _Dir . name %~ (++ suffix)

getFirstSubDirName :: FS -> Maybe String
getFirstSubDirName fs = fs ^? _Dir . contents . each . _Dir . name

getFileNames :: FS -> [String]
getFileNames fs = fs ^. _Dir . contents ^.. each . _File . name
