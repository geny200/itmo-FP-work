module Hw3.TSystem.TestExe
  ( -- * Function
    testSystem
  )
where

import Control.Lens ((.~), (^.))
import Control.Lens.Lens ((&))
import Control.Monad.Reader (ask, local, runReaderT)
import Control.Monad.State.Lazy (execState)
import Data.Maybe (fromMaybe)
import Hw3.FSystem.Command (commands)
import Hw3.FSystem.Data (ExplorerInfo (..), exit, output)
import Hw3.FSystem.FUtil (toPath)
import Hw3.Parser.Parser (runParser)
import Hw3.TSystem.System (TDirectory, dirName)
import Hw3.TSystem.TestFileSys (TestFS, fileSys)

-- | Function for executing commands in the file system
testSystem
  :: [String]     -- ^ list of commands to execute
  -> TDirectory   -- ^ initial state of the system
  -> TDirectory   -- ^ modified system
testSystem cmds curDir =
  let explorerInf = ExplorerInfo (toPath (curDir ^. dirName)) [] False
   in execState (runReaderT (fileSys (testExec cmds)) explorerInf) curDir

testExec :: [String] -> TestFS ()
testExec [] = return ()
testExec (cmd : xs) =
  do
    curr <- ask
    newInfo <-
      fromMaybe
        (return (curr & output .~ "command isn't recognized"))
        ( do
            (a, _) <- runParser commands cmd
            return (a curr)
        )
    if newInfo ^. exit || null xs
      then return ()
      else local (const (newInfo & output .~ [])) (testExec xs)
