module Hw4.TestTask7
  ( -- * Function
    fsTest
  )
where

import Control.Lens ((^.), (^..), (^?))
import Hw4.Task6 (FS (..))
import Hw4.Task7 (cd, file, ls)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests for Task7.
fsTest :: SpecWith ()
fsTest =
  describe "Task7 - filesys" $ do
    unitLsTest
    unitCdTest
    unitFileTest
    unitCommonTest

contentForFS :: [FS]
contentForFS =
  [ File "file1",
    Dir "subDir1" [],
    Dir
      "subDir2"
      [ Dir "sub2Dir1" [],
        Dir "sub2Dir2" [File "C"]
      ],
    File "file2"
  ]

testFS :: FS
testFS = Dir "someDir" contentForFS

-- | Unit tests for @ls@.
unitLsTest :: SpecWith ()
unitLsTest = it "ls test" $ do
  Dir "someDir" [] ^. ls `shouldBe` []
  File "someFile" ^. ls `shouldBe` []
  testFS ^. ls `shouldBe` contentForFS

-- | Unit tests for @cd@.
unitCdTest :: SpecWith ()
unitCdTest = it "cd test" $ do
  Dir "someDir" [] ^? cd "A" `shouldBe` Nothing
  File "someFile" ^? cd "A" `shouldBe` Nothing
  testFS ^? cd "subDir1" `shouldBe` Just (Dir "subDir1" [])
  testFS ^? cd "subDir2" . cd "sub2Dir1" `shouldBe` Just (Dir "sub2Dir1" [])

-- | Unit tests for @file@.
unitFileTest :: SpecWith ()
unitFileTest = it "file test" $ do
  Dir "someDir" [] ^? file "someFile" `shouldBe` Nothing
  File "someFile" ^? file "someFile" `shouldBe` Nothing
  testFS ^? file "someFile" `shouldBe` Nothing
  testFS ^? file "file2" `shouldBe` Just "file2"
  testFS ^? file "file1" `shouldBe` Just "file1"

-- | Unit tests for @file@, @cd@ and @ls@.
unitCommonTest :: SpecWith ()
unitCommonTest = it "common test" $ do
  testFS ^? cd "subDir2" . cd "sub2Dir1" . file "C" `shouldBe` Nothing
  testFS ^? cd "subDir2" . cd "sub2Dir2" . file "C" `shouldBe` Just "C"
  testFS ^.. cd "subDir2" . ls
    `shouldBe` [[Dir "sub2Dir1" [], Dir "sub2Dir2" [File "C"]]]
