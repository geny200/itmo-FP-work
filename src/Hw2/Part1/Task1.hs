module Hw2.Part1.Task1
  ( -- * Function
    stringSum
  )
where

import Data.List.Split  (splitOn)
import Text.Read        (readMaybe)

-- | The function sums the numbers in a string. 
-- Returns @Just Int@ with the result if the input `String` was valid
-- and @Nothing@ otherwise.
stringSum 
  :: String     -- ^ input string with numbers
  -> Maybe Int  -- ^ the result of the summing
stringSum str
 = do
    let tokens = filter (not . null) (splitOn " " str)
    ints <- traverse readMaybe tokens
    return (sum ints)
