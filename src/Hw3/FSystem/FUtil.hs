module Hw3.FSystem.FUtil
  ( -- * Functions
    createPath
  , createUpPath
  , fromPath
  , getNewPath
  , toPath
  )
where

import Control.Lens ((^.), _head)
import Filesystem.Path (FilePath, append, collapse)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Hw3.FSystem.Data (ExecutorInfo, argument, currentDir, explorer)
import Prelude hiding (FilePath)

-- | Creates a path based on the current directory 
-- and argument (which can be either an absolute or relative path)
createPath :: ExecutorInfo -> FilePath
createPath exec = 
  getNewPath 
    (exec ^. explorer . currentDir) 
    (toPath (exec ^. argument . _head))

-- | Returns the path to the parent folder
createUpPath :: FilePath -> FilePath
createUpPath path = 
  getNewPath 
  path 
  (toPath "..")

-- | Creates a new path based on combining the two
getNewPath :: FilePath -> FilePath -> FilePath
getNewPath current newPath = collapse (append current newPath)

-- | Converts a string to a file path
toPath :: String -> FilePath
toPath = decodeString

-- | Converts a file path to a string
fromPath :: FilePath -> String
fromPath = encodeString
