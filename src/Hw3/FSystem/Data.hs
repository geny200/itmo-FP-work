{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Hw3.FSystem.Data
  ( -- * Constructors Explorer
    Explorer (..)
    
    -- * Constructors ExplorerInfo
  , ExplorerInfo (..)
  
    -- * Constructors ExecutorInfo
  , ExecutorInfo (..)
  
    -- * Constructors DataInfo
  , DataInfo (..)
  
    -- * Constructors FileInfo
  , FileInfo (..)
  
    -- * Constructors DirInfo
  , DirInfo (..)
  
    -- * Lens
  , access
  , argument
  , currentDir
  , defaultDataInfo
  , explorer
  , exit
  , modifyTime
  , output
  , path
  , size
  , showPermissions
  )
where

import Control.Lens (makeLenses, (&), (^.))
import Data.Time (UTCTime (..), fromGregorian)
import Filesystem.Path (FilePath, empty)
import Filesystem.Path.CurrentOS (encodeString)
import System.Directory (emptyPermissions)
import System.Directory.Internal (Permissions (..))
import Prelude hiding (FilePath)

class (Monad m) => Explorer m where
  -- | Checks for the existence of a directory. 
  -- Returns `True` if the directory exists 
  -- `False` otherwise
  testDir :: FilePath -> m Bool
  
  -- | Checks for the existence of a file. 
  -- Returns `True` if the file exists 
  -- `False` otherwise
  testFil :: FilePath -> m Bool
  
  -- | Returns a list of the entire contents 
  -- of the directory.
  listDir :: FilePath -> m [String]
  
  -- | Deletes a file or directory.
  removeFD :: FilePath -> m ()
  
  -- | Creates a directory.
  createDir :: FilePath -> m ()
  
  -- | Writes data to a file. If the file 
  -- existed, it overwrites it, otherwise 
  -- it creates and writes to new file.
  recordFil :: String -> FilePath -> m ()
  
  -- | Reads the entire contents of the file.
  readFil :: FilePath -> m String
  
  -- | Returns the time of the last 
  -- modification of a directory or file.
  timeMod :: FilePath -> m UTCTime
  
  -- | Returns the file size in bytes.
  sizeFil :: FilePath -> m Integer
  
  -- | Returns access rights to a 
  -- file or directory.
  permiss :: FilePath -> m Permissions

-- | Contains information about the file.
data FileInfo = FileInfo
  { _extension :: String,     -- ^ extension ("-" if absent)
    _fileCommon :: DataInfo   -- ^ common meta-information
  }

-- | Contains information about the directory.
data DirInfo = DirInfo
  { _countFiles :: Integer,   -- ^ number of files contained (recursively)
    _dirCommon :: DataInfo    -- ^ common meta-information
  }

-- | Common meta-information
data DataInfo = DataInfo
  { _path :: FilePath,        -- ^ absolute path to the element
    _access :: Permissions,   -- ^ access rights
    _modifyTime :: UTCTime,   -- ^ time of the last modification
    _size :: Integer          -- ^ size in bytes
  }

-- | Information about the current state of the Explorer
data ExplorerInfo = ExplorerInfo
  { _currentDir :: FilePath,  -- ^ current directory
    _output :: String,        -- ^ current output
    _exit :: Bool             -- ^ exit status
  }

-- | Information for executable commands
data ExecutorInfo = ExecutorInfo
  { _argument :: [String],    -- ^ command arguments
    _explorer :: ExplorerInfo -- ^ current state of the Explorer
  }

makeLenses ''ExecutorInfo
makeLenses ''ExplorerInfo
makeLenses ''DataInfo
makeLenses ''DirInfo
makeLenses ''FileInfo

instance Show DirInfo where
  show :: DirInfo -> String
  show dir =
    (dir ^. dirCommon & show)
      ++ "\nnumber of files: "
      ++ (dir ^. countFiles & show)

instance Show FileInfo where
  show :: FileInfo -> String
  show file =
    (file ^. fileCommon & show)
      ++ "\nextension: "
      ++ (file ^. extension & show)

instance Show DataInfo where
  show :: DataInfo -> String
  show inform =
    "path: " ++ (inform ^. path & show . encodeString)
      ++ "\nmodification time: "
      ++ (inform ^. modifyTime & show)
      ++ "\nsize: "
      ++ (inform ^. size & show)
      ++ " byte"
      ++ "\naccess: "
      ++ (inform ^. access & showPermissions)

showAlpha :: Bool -> Char -> Char
showAlpha False _ = '_'
showAlpha True c = c

showPermissions :: Permissions -> String
showPermissions (Permissions r w e s) =
  [showAlpha r 'r', showAlpha w 'w', showAlpha e 'e', showAlpha s 's']

defaultDataInfo :: DataInfo
defaultDataInfo = 
  DataInfo empty emptyPermissions (UTCTime (fromGregorian 1971 01 01) 0) 0
