{-# LANGUAGE RankNTypes #-}

module Hw4.Task7 
  ( -- * Traversals
    cd
  , file
  , ls
  )
where

import Lens.Micro (Traversal', each, filtered, (^.))
import Hw4.Task6 (FS (..), contents, name, _Dir, _File)

-- | Get a list of directory content names.
ls :: Traversal' FS [FS]
ls = _Dir . contents

-- | Go to the subdirectory with the specified name.
cd :: FilePath -> Traversal' FS FS
cd dirName =
  ls . each
    . filtered (\x -> (x ^. _Dir . name) == dirName)

-- | Get the name of a specific File, if it exists.
file :: FilePath -> Traversal' FS FilePath
file fileName =
  ls . each
    . filtered (\x -> (x ^. _File . name) == fileName)
    . name
