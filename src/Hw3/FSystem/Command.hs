{-# LANGUAGE BangPatterns #-}

module Hw3.FSystem.Command
  ( -- * Parser
    commands
  )
where

import Control.Applicative ((<|>))
import Control.Category ((<<<), (>>>))
import Control.Lens (each, ix, (%~), (&), (.=), (.~), (^.), _head)
import Control.Monad.State.Lazy (StateT, filterM, forM, lift, runStateT)
import Data.Text (unpack)
import Filesystem.Path (FilePath, extension, filename)
import Hw3.FSystem.Data -- import all functions
import Hw3.FSystem.FUtil (createPath, createUpPath, fromPath, getNewPath, toPath)
import Hw3.FSystem.MessageText -- import all functions
import Hw3.Parser.Combinator (greedily, space, stream, wordText)
import Hw3.Parser.Parser (Parser (..))
import Hw3.Util (join)
import Prelude hiding (FilePath, writeFile)

-- | Parser of all supported commands
commands
  :: (Explorer m)
  => Parser Char (ExplorerInfo -> m ExplorerInfo)
commands =
  space
    >> (       (stream "cd" >> parseArgument cd)
           <|> (stream "dir" >> parseArgument dir)
           <|> (stream "ls" >> parseArgument ls)
           <|> (stream "cat" >> parseArgument cat)
           <|> (stream "exit" >> parseArgument ext)
           <|> (stream "remove" >> parseArgument remove)
           <|> (stream "create-file" >> parseArgument creatFile)
           <|> (stream "create-folder" >> parseArgument creatFolder)
           <|> (stream "write-file" >> parseArgument writeFile)
           <|> (stream "help" >> parseArgument help)
           <|> (stream "information" >> parseArgument information)
           <|> (stream "find-file" >> parseArgument findFile)
       )

-----------------------------------------
--     common auxiliary functions     ---
-----------------------------------------

type CheckExecutor m 
  = (ExecutorInfo -> m ExplorerInfo) 
  -> ExecutorInfo 
  -> m ExplorerInfo

commonTest 
  :: (Explorer m) 
  => (FilePath -> m Bool) 
  -> Bool 
  -> String 
  -> CheckExecutor m
commonTest testFS flag str action exec =
  do
    let newPath = createPath exec
    exists <- testFS newPath
    if exists == flag
      then action (exec & explorer . currentDir .~ newPath)
      else return (exec ^. explorer & output .~ str)

testExistDir :: (Explorer m) => CheckExecutor m
testExistDir = commonTest testDir True msgDirDoesn'tExist

testNotExistDir :: (Explorer m) => CheckExecutor m
testNotExistDir = commonTest testDir False msgDirAlreadyExist

testExistFile :: (Explorer m) => CheckExecutor m
testExistFile = commonTest testFil True msgFilDoesn'tExist

testNotExistFile :: (Explorer m) => CheckExecutor m
testNotExistFile = commonTest testFil False msgFilAlreadyExist

testExist :: (Explorer m) => CheckExecutor m
testExist = 
  commonTest 
  (\x -> (||) <$> testFil x <*> testDir x) 
  True 
  msgDirOrFileDoesn'tExist

saveContext :: (Explorer m) => CheckExecutor m
saveContext action exec =
  do
    result <- action exec
    return (result & currentDir .~ (exec ^. (explorer . currentDir)))

-----------------------------------------
--            command functions       ---
-----------------------------------------

cd :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
cd = testExistDir (\x -> return (x ^. explorer))

dir :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
dir exec =
  do
    content <- listDir (exec ^. explorer . currentDir)
    return (exec ^. explorer & output .~ join "\n" content)

ls :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
ls = saveContext (testExistDir dir)

cat :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
cat =
  saveContext
    ( testExistFile
        ( \x ->
            do
              str <- readFil (x ^. explorer . currentDir)
              return (x ^. explorer & output .~ str)
        )
    )

ext :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
ext exec = return (exec ^. explorer & exit .~ True & output .~ msgExit)

creatFile :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
creatFile =
  saveContext
    ( testNotExistFile
        ( \x ->
            do
              recordFil [] (x ^. explorer . currentDir)
              return (x ^. explorer)
        )
    )

writeFile :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
writeFile =
  saveContext
    ( testExistFile
        ( \x ->
            do
              recordFil (x ^. argument . ix 1) (x ^. explorer . currentDir)
              return (x ^. explorer)
        )
    )

creatFolder :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
creatFolder =
  saveContext
    ( testNotExistDir
        ( \x ->
            do
              createDir (x ^. explorer . currentDir)
              return (x ^. explorer)
        )
    )

remove :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
remove exec =
  testExist
    ( \x ->
        do
          let rPath = x ^. explorer . currentDir
          removeFD rPath
          return
            ( exec ^. explorer
                & ( if rPath == (exec ^. (explorer . currentDir))
                      then currentDir %~ createUpPath
                      else currentDir .~ (exec ^. (explorer . currentDir))
                  )
            )
    )
    exec

listAbsDir :: (Explorer m) => FilePath -> m [FilePath]
listAbsDir rootPath =
  do
    names <- listDir rootPath
    return (names & each %~ (getNewPath rootPath . toPath))

recursive 
  :: (Explorer m) 
  => (FilePath -> m c) 
  -> (c -> c -> c) 
  -> [FilePath] 
  -> c 
  -> m c
recursive _ _ [] !acc = return acc
recursive action combine !rootPath !acc =
  do
    allContents <- forM rootPath listAbsDir
    let content = concat allContents
    dirs <- filterM testDir content
    fils <- filterM testFil content
    results <- forM fils action
    recursive action combine dirs (foldr1 combine (acc : results))

getCommonInfo :: (Explorer m) => FilePath -> StateT DataInfo m ()
getCommonInfo newPath =
  do
    time <- lift (timeMod newPath)
    permissions <- lift (permiss newPath)
    dataSize <- lift (sizeFil newPath)
    path .= newPath
    modifyTime .= time
    access .= permissions
    size .= dataSize
    pure ()

information :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
information exec =
  testExist
    ( \x ->
        do
          let newPath = x ^. explorer . currentDir
          (_, res) <- runStateT (getCommonInfo newPath) defaultDataInfo
          isDir <- testDir newPath
          if isDir
            then do
              count <- recursive (return . const 1) (+) [newPath] 0
              dataSize <- recursive sizeFil (+) [newPath] 0
              return
                ( exec ^. explorer
                    & output .~ (DirInfo count (res & size .~ dataSize) & show)
                )
            else
              let extens = maybe "-" unpack (extension newPath)
               in return
                    ( exec ^. explorer
                        & output .~ (FileInfo extens res & show)
                    )
    )
    exec

findFile :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
findFile exec =
  do
    let nameF = filename . toPath $ (exec ^. argument . _head)
    files <-
      recursive
        (\x -> return ([x | filename x == nameF]))
        (++)
        [exec ^. explorer . currentDir]
        []
    return (exec ^. explorer & output .~ join "\n" (files & each %~ fromPath))

help :: (Explorer m) => ExecutorInfo -> m ExplorerInfo
help exec = return (exec ^. explorer & output .~ msgHelp)

parseArgument :: (ExecutorInfo -> c) -> Parser Char (ExplorerInfo -> c)
parseArgument action =
  (>>>) (ExecutorInfo []) . (<<<) action
    <$> greedily ((\arg -> argument %~ (arg :)) <$> (space >> wordText))
