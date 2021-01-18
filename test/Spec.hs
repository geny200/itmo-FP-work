import Hw1.TestHw1 (testHw1)
import Hw2.TestHw2 (testHw2)
import Hw3.TestHw3 (testHw3)
import Hw4.TestHw4 (testHw4)
import Test.Hspec (hspec)

main :: IO ()
main =
  do
    testHw1
    hspec $ do
      testHw2
      testHw3
      testHw4
