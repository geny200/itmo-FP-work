module Hw3.SimpleTest
  ( -- * Test for TestFS
    testFileSys
  )
where

import Control.Lens ((%~), (.~), _head)
import Control.Lens.Lens ((&))
import Hw3.TSystem.System (TDirectory (..), content, defaultTDir, defaultTFile, dirName, directories, fileName, files)
import Hw3.TSystem.TestExe (testSystem)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | Unit tests
testFileSys :: SpecWith ()
testFileSys = describe "testFileSys - common test" $ do
  uTest1
  uTest2
  uTest3
  uTest4
  uTest5
  uTest6
  
-----------------------------------------------------------
----                    Data for tests                -----
-----------------------------------------------------------

dirWithFile :: String -> [String] -> TDirectory
dirWithFile name filesName =
  defaultTDir
    & dirName .~ name
    & files .~ map (\x -> defaultTFile & fileName .~ x) filesName

dirWithFolder :: String -> [TDirectory] -> TDirectory
dirWithFolder name folders =
  defaultTDir
    & dirName .~ name
    & directories .~ folders

emptyRoot :: TDirectory
emptyRoot = defaultTDir & dirName .~ "D:\\"

testInput :: TDirectory
testInput =
  emptyRoot & directories
    .~ [ dirWithFile "a" ["a.txt", "aa.txt"],
         dirWithFile "b" ["bb.txt", "aa.txt"],
         dirWithFile "c" ["a.txt", "c.txt"]
       ]
       
-----------------------------------------------------------
----                    Unit tests                    -----
-----------------------------------------------------------

uTest1 :: SpecWith ()
uTest1 = it "nothing change" $ do
  testSystem
    [ "create-folder a",
      "cd a",
      "exit",
      "create-folder trash"
    ]
    testInput
    `shouldBe` testInput

uTest2 :: SpecWith ()
uTest2 = it "new folder" $ do
  testSystem
    [ "create-folder e",
      "exit"
    ]
    testInput
    `shouldBe` ( testInput
                   & directories %~ ((defaultTDir & dirName .~ "e") :)
               )

uTest3 :: SpecWith ()
uTest3 = it "new folder in new folder" $ do
  testSystem
    [ "create-folder e",
      "cd e",
      "create-folder f",
      "exit"
    ]
    testInput
    `shouldBe` ( testInput
                   & directories %~ (dirWithFolder "e" [dirWithFolder "f" []] :)
               )

uTest4 :: SpecWith ()
uTest4 = it "new file and write file" $ do
  testSystem
    [ "create-folder first",
      "cd first",
      "create-file test.txt",
      "write-file test.txt \"some text here\"",
      "exit"
    ]
    testInput
    `shouldBe` ( testInput
                   & directories
                     %~ ( ( dirWithFile "first" ["test.txt"]
                              & files . _head %~ content .~ "some text here"
                          )
                            :
                        )
               )

uTest5 :: SpecWith ()
uTest5 = it "delete new folder" $ do
  testSystem
    [ "create-folder e",
      "remove e",
      "exit"
    ]
    testInput
    `shouldBe` testInput

uTest6 :: SpecWith ()
uTest6 = it "delete new file" $ do
  testSystem
    [ "cd a",
      "create-file test.txt",
      "remove test.txt",
      "exit"
    ]
    testInput
    `shouldBe` testInput
