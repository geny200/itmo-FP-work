module Hw4.TestTask6
  ( -- * Function
    lensTest
  )
where

import Control.Lens ((^.))
import Hw4.Task6
  ( FS (..),
    addSuffixRoot,
    changeRootName,
    contents,
    directoryName,
    fileName,
    getFileNames,
    getFirstSubDirName,
    getSubDirs,
    name,
  )
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests for Task6.
lensTest :: SpecWith ()
lensTest =
  describe "Task6 - lens" $ do
    unitNameTest
    unitContentTest
    unitPrismTest
    unitCommonTest

contentForFS :: [FS]
contentForFS =
  [ File "file1",
    Dir "subDir1" [],
    Dir "subDir2" [],
    File "file2"
  ]

testFS :: FS
testFS = Dir "someDir" contentForFS

-- | Unit tests for `name` lens.
unitNameTest :: SpecWith ()
unitNameTest = it "name test" $ do
  Dir "someDir" [] ^. name `shouldBe` "someDir"
  File "someFile" ^. name `shouldBe` "someFile"

-- | Unit tests for `contents` lens.
unitContentTest :: SpecWith ()
unitContentTest = it "content test" $ do
  Dir "someDir" [] ^. contents `shouldBe` []
  testFS ^. contents `shouldBe` contentForFS
  File "someFile" ^. contents `shouldBe` []

-- | Unit tests for `_Dir` and `_File` prisms.
unitPrismTest :: SpecWith ()
unitPrismTest = it "prisms test" $ do
  directoryName (Dir "SomeDir" []) `shouldBe` Just "SomeDir"
  directoryName (File "someFile") `shouldBe` Nothing
  fileName (File "someFile") `shouldBe` "someFile"
  fileName (Dir "someDir" []) `shouldBe` ""

-- | Unit tests for `_Dir`, `_File` prisms and `name`, `contents` lenses.
unitCommonTest :: SpecWith ()
unitCommonTest = it "common test" $ do
  getSubDirs (Dir "someDir" []) `shouldBe` []
  getSubDirs (File "someFile") `shouldBe` []
  getSubDirs testFS `shouldBe` [Dir "subDir1" [], Dir "subDir2" []]

  changeRootName (File "someFile") `shouldBe` File "someFile"
  changeRootName (Dir "someFile" []) `shouldBe` Dir "/" []

  addSuffixRoot (File "someFile") "Suffix" `shouldBe` File "someFile"
  addSuffixRoot (Dir "someDir" []) "Suffix" `shouldBe` Dir "someDirSuffix" []

  getFirstSubDirName (Dir "someDir" []) `shouldBe` Nothing
  getFirstSubDirName (File "someFile") `shouldBe` Nothing
  getFirstSubDirName testFS `shouldBe` Just "subDir1"

  getFileNames (Dir "someDir" []) `shouldBe` []
  getFileNames (File "someFile") `shouldBe` []
  getFileNames testFS `shouldBe` ["file1", "file2"]
