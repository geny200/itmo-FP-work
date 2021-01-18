module Hw3.Util where

import Data.List (intercalate)

-- | Concat the list by embedding a delimeter between elements.
join 
  :: [a]      -- ^ delimiter
  -> [[a]]    -- ^ list of lists
  -> [a]      -- ^ the result of concatenation
join = intercalate
