{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Hw3.TSystem.TestFileSys 
  ( -- * Constructors TestFS
    TestFS (..)
  )
where

import Control.Lens (each, filtered, ix, (%~), (&), (.~), (^.), (^..), (^?))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Lazy (MonadState, State, get, modify)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Filesystem.Path (FilePath, append, basename, directory, dirname, filename, parent, splitDirectories)
import Hw3.FSystem.Data (Explorer (..), ExplorerInfo (..))
import Hw3.FSystem.FUtil (fromPath)
import Hw3.TSystem.System
  ( TDirectory (..),
    TFile (..),
    content,
    defaultTDir,
    defaultTFile,
    defaultTTime,
    dirName,
    directories,
    fileName,
    files,
    metaDir,
    metaFil,
    permission,
    size,
    timeModification,
  )
import System.Directory (Permissions, emptyPermissions)
import Prelude hiding (FilePath)

-- | @TestFS@ - not a real file system 
newtype TestFS a = TestFS {fileSys :: ReaderT ExplorerInfo (State TDirectory) a}
  deriving (Functor, Applicative, Monad, MonadState TDirectory, MonadReader ExplorerInfo)

instance Explorer TestFS where
  testDir :: FilePath -> TestFS Bool
  testDir = infoDir (const True) False

  testFil :: FilePath -> TestFS Bool
  testFil = infoFile (const True) False

  listDir :: FilePath -> TestFS [String]
  listDir = infoDir (\x -> x ^. directories ^.. each . dirName) []

  createDir :: FilePath -> TestFS ()
  createDir path =
    actionDir
      (directories %~ ((defaultTDir & dirName .~ fromPath (basename path)) :))
      path

  recordFil :: String -> FilePath -> TestFS ()
  recordFil str path =
    actionDir
      ( \x ->
          let name = fromPath (filename path)
           in case elemIndex name (x ^. files ^.. each . fileName) of
                Nothing -> x & files %~ ((defaultTFile & fileName .~ name & content .~ str) :)
                (Just index) -> x & files . ix index %~ (content .~ str)
      )
      path

  readFil :: FilePath -> TestFS String
  readFil = infoFile (^. content) []

  removeFD :: FilePath -> TestFS ()
  removeFD path =
    actionDir
      ( \x ->
          let name = fromPath (filename path)
           in x
                & files .~ (x ^. files ^.. each . filtered (\y -> (y ^. fileName) /= name))
                & directories .~ (x ^. directories ^.. each . filtered (\y -> (y ^. dirName) /= name))
      )
      (append (parent path) (filename path))

  permiss :: FilePath -> TestFS Permissions
  permiss path =
    do
      t <- infoDir (\x -> x ^. (metaDir . permission)) emptyPermissions path
      infoFile (\x -> x ^. (metaFil . permission)) t path

  sizeFil :: FilePath -> TestFS Integer
  sizeFil = infoFile (\x -> x ^. metaFil . size) 0

  timeMod :: FilePath -> TestFS UTCTime
  timeMod path = do
    t <- infoDir (\x -> x ^. (metaDir . timeModification)) defaultTTime path
    infoFile (\x -> x ^. (metaFil . timeModification)) t path

splitPath :: FilePath -> [String]
splitPath path = tail (map (fromPath . dirname) (splitDirectories . directory $ path))

findDirectory :: String -> TDirectory -> Maybe Int
findDirectory x currentDir = elemIndex x (currentDir ^. directories ^.. each . dirName)

overDir :: [String] -> (TDirectory -> TDirectory) -> TDirectory -> TDirectory
overDir [] action currentDir = action currentDir
overDir (x : xs) action currentDir =
  let indexNext = fromMaybe (-1) (findDirectory x currentDir)
   in (currentDir & (directories . ix indexNext %~ overDir xs action))

viewDir :: [String] -> TDirectory -> Maybe TDirectory
viewDir [] dir = Just dir
viewDir [x] currentDir =
  do
    indexNext <- findDirectory x currentDir
    currentDir ^. directories ^? ix indexNext
viewDir (x : xs) currentDir =
  do
    indexNext <- findDirectory x currentDir
    nextDir <- currentDir ^. directories ^? ix indexNext
    viewDir xs nextDir

viewPathDir :: FilePath -> TDirectory -> Maybe TDirectory
viewPathDir fPath = viewDir (splitPath fPath ++ [fromPath (basename fPath)])

viewPathFil :: FilePath -> TDirectory -> Maybe TFile
viewPathFil fPath computer =
  do
    currentDir <- viewPathDir (append (parent . directory $ fPath) (dirname fPath)) computer
    let name = fromPath (filename fPath)
    indexNext <- elemIndex name (currentDir ^. files ^.. each . fileName)
    currentDir ^. files ^? ix indexNext

infoCommon :: (FilePath -> TDirectory -> Maybe a) -> (a -> c) -> c -> FilePath -> TestFS c
infoCommon viewCommon action def path =
  ( \x ->
      fromMaybe
        def
        ( do
            currentFil <- x
            return (currentFil & action)
        )
  )
    <$> (viewCommon path <$> get)

infoFile :: (TFile -> c) -> c -> FilePath -> TestFS c
infoFile = infoCommon viewPathFil

infoDir :: (TDirectory -> c) -> c -> FilePath -> TestFS c
infoDir = infoCommon viewPathDir

actionDir :: (TDirectory -> TDirectory) -> FilePath -> TestFS ()
actionDir action path = modify (overDir (splitPath path) action)
