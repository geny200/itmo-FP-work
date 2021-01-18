module Hw2.TestPart3.TestParser
  ( -- * Functions
    testFail
  , testList
  , testListJust
  , testJust
  , testJustAll
  )
where

import Hw2.Part3.Task1 (Parser (..))
import Test.Hspec (shouldBe)

-- | Starts and checks the `Just` output of the parser.
testJust
  :: (Eq a, Eq s, Show a, Show s)
  => Parser s a     -- ^ the parser to run
  -> [s]            -- ^ the input for the parser
  -> a              -- ^ expected result of the parser
  -> [s]            -- ^ expected remainder of the list
  -> IO ()          -- ^ fail if it doesn't match result and expected
testJust parser str x xs = runParser parser str `shouldBe` Just (x, xs)

-- | Starts and checks the `Just` output of the parser 
-- without the rest of the list
testJustAll 
  :: (Eq a, Eq s, Show a, Show s) 
  => Parser s a     -- ^ the parser to run
  -> [s]            -- ^ the input for the parser
  -> a              -- ^ expected result of the parser
  -> IO ()          -- ^ fail if it doesn't match result and expected
testJustAll parser str x = testJust parser str x []

-- | Starts and checks the `Nothing` output of the parser.
testFail 
  :: (Eq a, Eq s, Show a, Show s) 
  => Parser s a     -- ^ the parser to run
  -> [s]            -- ^ the input for the parser
  -> IO ()          -- ^ fail if result of parser was `Just`
testFail parser str = runParser parser str `shouldBe` Nothing

-- | Runs and checks the output of the parser 
-- without the rest of the list, for the entire 
-- list of input streams.
testList 
  :: ([s] -> IO ()) -- ^ function for starting the parser
  -> [[s]]          -- ^ list of input list for testing
  -> IO ()          -- ^ failure if at least one failed
testList f = foldr ((>>) . f) (pure ())

-- | Runs and checks the output of the parser 
-- without the rest of the list, but with result value, 
-- for the entire list of input streams.
testListJust 
  :: ([s] -> a -> IO ()) -- ^ function for starting the parser
  -> [([s], a)]          -- ^ list of (input list, result) for testing
  -> IO ()               -- ^ failure if at least one failed
testListJust f = foldr ((>>) . uncurry f) (pure ())
