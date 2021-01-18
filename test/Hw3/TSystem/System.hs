{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Hw3.TSystem.System
  ( -- * Constructors TDirectory
    TDirectory (..)

    -- * Constructors TFile
  , TFile (..)

    -- * Constructors TMetaData
  , TMetaData (..)

    -- * Default values
  , defaultMetaData
  , defaultTDir
  , defaultTFile
  , defaultTTime

    -- * Lens
  , content
  , directories
  , dirName
  , fileName
  , files
  , metaDir
  , metaFil
  , permission
  , size
  , timeModification
  )
where

import Control.Lens (makeLenses)
import Data.Time (UTCTime (..), fromGregorian)
import Hw3.FSystem.Data (showPermissions)
import Hw3.Util (join)
import System.Directory (emptyPermissions)
import System.Directory.Internal (Permissions (..))
import Prelude hiding (FilePath)

-- | Simulates the behavior of file system content
data TDirectory = TDirectory
  { _dirName :: String,           -- ^ the name of the directory
    _directories :: [TDirectory], -- ^ list of subdirectories
    _files :: [TFile],            -- ^ list of contained files
    _metaDir :: TMetaData         -- ^ meta-information
  }
  deriving (Show, Eq)

data TFile = TFile
  { _fileName :: String,          -- ^ the name of the file
    _content :: String,           -- ^ the contents of the file
    _metaFil :: TMetaData         -- ^ meta-information
  }
  deriving (Show, Eq)

data TMetaData = TMetaData
  { _permission :: Permissions,   -- ^ permissions for a given folder or file
    _timeModification :: UTCTime, -- ^ time of last modification
    _size :: Integer              -- ^ file size
  }
  deriving (Eq)

instance Show TMetaData where
  show :: TMetaData -> String
  show (TMetaData access time size) =
    join "; " [showPermissions access, show time, show size ++ " byte"]

makeLenses ''TDirectory
makeLenses ''TFile
makeLenses ''TMetaData

defaultTDir :: TDirectory
defaultTDir = TDirectory "dir" [] [] defaultMetaData

defaultTFile :: TFile
defaultTFile = TFile "file.test" [] defaultMetaData

defaultTTime :: UTCTime
defaultTTime = UTCTime (fromGregorian 1971 01 01) 0

defaultMetaData :: TMetaData
defaultMetaData = TMetaData emptyPermissions (UTCTime (fromGregorian 1971 01 01) 0) 0
