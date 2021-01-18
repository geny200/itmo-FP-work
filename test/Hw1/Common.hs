module Hw1.Common
  ( -- * Functions
    myTest
  , myTestMap
  ) where
import Test.HUnit

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
myTest 
  :: (Eq a, Show a) 
  => String     -- ^ message
  -> a          -- ^ expected value
  -> a          -- ^ actual value
  -> Test       -- ^ resulting `Test`
myTest name x y 
  = TestCase (assertEqual (foldr1 (++) 
    ["for (", name, " ", show x, " ", show y, "),"]) 
    x y)

-- | Asserts that the specified actual values returned by second function
-- is equal to the expected values returned by the first function.
myTestMap :: 
  (Eq a, Show a) 
  => String 
  -> (b -> a)   -- ^ first function returns the expected value
  -> (b -> a)   -- ^ second function returns the actual value
  -> [b]        -- ^ list to map
  -> Test       -- ^ resulting `Test`
myTestMap name x y listData = TestList (map 
    (\list -> myTest name (x list) (y list)) listData)