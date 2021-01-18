{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Hw3.FSystem.RealFS 
  ( -- * Constructors RealFS
    RealFS (..)
  )
where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Time (UTCTime)
import qualified Filesystem.Path as FS (FilePath)
import Hw3.FSystem.Data (Explorer (..), ExplorerInfo (..))
import Hw3.FSystem.FUtil (fromPath)
import System.Directory
  ( Permissions,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getModificationTime,
    getPermissions,
    listDirectory,
    removePathForcibly,
  )
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

-- | @RealFS@ - interacts with the real file system
newtype RealFS a = RealFS {fileSys :: ReaderT ExplorerInfo IO a}
  deriving (Functor, Applicative, Monad, MonadReader ExplorerInfo, MonadIO)

instance Explorer RealFS where
  testDir :: FS.FilePath -> RealFS Bool
  testDir path = liftIO (doesDirectoryExist (fromPath path))

  testFil :: FS.FilePath -> RealFS Bool
  testFil path = liftIO (doesFileExist (fromPath path))

  listDir :: FS.FilePath -> RealFS [String]
  listDir path = liftIO (listDirectory (fromPath path))

  createDir :: FS.FilePath -> RealFS ()
  createDir path = liftIO (createDirectory (fromPath path))

  recordFil :: String -> FS.FilePath -> RealFS ()
  recordFil str path = liftIO (writeFile (fromPath path) str)

  readFil :: FS.FilePath -> RealFS String
  readFil path =
    liftIO
      ( do
          let strPath = fromPath path
          handle <- openFile strPath ReadMode
          everything <- hGetContents handle
          evaluate (rnf everything)
          hClose handle
          return everything
      )

  removeFD :: FS.FilePath -> RealFS ()
  removeFD path = liftIO (removePathForcibly (fromPath path))

  permiss :: FS.FilePath -> RealFS Permissions
  permiss path = liftIO (getPermissions (fromPath path))

  sizeFil :: FS.FilePath -> RealFS Integer
  sizeFil path = liftIO (getFileSize (fromPath path))

  timeMod :: FS.FilePath -> RealFS UTCTime
  timeMod path = liftIO (getModificationTime (fromPath path))
