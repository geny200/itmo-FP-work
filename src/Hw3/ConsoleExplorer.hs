module Hw3.ConsoleExplorer
  ( -- * Function
    consoleExplorer
  )
where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (liftIO)
import Control.Monad.RWS.Class (ask, local)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Hw3.FSystem.Command (commands)
import Hw3.FSystem.Data (ExplorerInfo (..), currentDir, exit, output)
import Hw3.FSystem.FUtil (fromPath, toPath)
import Hw3.FSystem.RealFS (RealFS, fileSys)
import Hw3.Parser.Parser (Parser (..))
import System.Directory (getCurrentDirectory)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- | Function to run the console version of the file manager.
consoleExplorer :: IO ()
consoleExplorer =
  do
    curDir <- getCurrentDirectory
    let info = ExplorerInfo (toPath curDir) [] False
    hSetBuffering stdout NoBuffering
    putStrLn "Sheol File Manager, version 1.0.0"
    runReaderT (fileSys consoleExp) info

-- | Environment of communication with the user.
consoleExp :: RealFS ()
consoleExp =
  do
    curr <- ask
    liftIO $ putStr (fromPath (curr ^. currentDir) ++ ">")
    command <- liftIO getLine
    newInfo <-
      fromMaybe
        (return (curr & output .~ "command isn't recognized"))
        ( do
            (a, _) <- runParser commands command
            return (a curr)
        )
    liftIO $ putStrLn (newInfo ^. output)
    if newInfo ^. exit
      then pure ()
      else local (const (newInfo & output .~ [])) consoleExp
