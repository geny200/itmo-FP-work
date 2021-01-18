module Hw1.Part1.Task3
  ( -- * `Three` constructors
    Three (Leaf, Node)
    
    -- * Functions
  , findF
  , fromList
  , insertF
  , isEmpty
  , lengthF
  , remove
  ) where

data Three a
  = Node (Three a) [a] (Three a)  -- ^ Node with two with two kids `Three` and a list
  | Leaf                          -- ^ Empty Node
  deriving Show                   -- ^ default 'Show' instance

-- | Checks the `Three` for emptiness.
isEmpty 
  :: Three a  -- ^ given `Three`
  -> Bool     -- ^ @True@ if the `Three` is empty
isEmpty Leaf = True
isEmpty _ = False

-- | Returns the number of elements in the `Three`.
lengthF 
  :: (Ord a) 
  => Three a  -- ^ given `Three`
  -> Int      -- ^ number of elements in the given `Three`
lengthF Leaf = 0
lengthF (Node left list right) 
  = length list + lengthF left + lengthF right

-- | Checks if an element is contained in the tree.
findF 
  :: (Ord a) 
  => Three a  -- ^ given `Three`
  -> a        -- ^ element to search for
  -> Bool     -- ^ @True@ if given `Three` contained element, @False@ otherwise
findF Leaf _ = False
findF (Node left (element : _) right) e
  | e < element = findF left e
  | e > element = findF right e
  | otherwise   = True
findF (Node _ [] _) _ = undefined

-- | Inserts an element into the `Three`.
insertF
  :: (Ord a) 
  => Three a  -- ^ given `Three`
  -> a        -- ^ element to insert
  -> Three a  -- ^ modified `Three`
insertF= editThree _insert

-- | Removes an element from the `Three`.
remove 
  :: (Ord a) 
  => Three a  -- ^ given `Three`
  -> a        -- ^ element to remove
  -> Three a  -- ^ modified `Three`
remove = editThree _delete

-- | Creates a `Three` from a [@a@].
fromList 
  :: (Ord a) 
  => [a]      -- ^ given list [@a@]
  -> Three a  -- ^ created `Three`
fromList = foldl insertF Leaf

------------------------
-- Secondary functions
------------------------

editThree 
  :: (Ord a) => (Three a -> a -> Three a) -> Three a -> a -> Three a
editThree f Leaf e = f Leaf e
editThree f (Node left list right) e
  | e < head list = Node (editThree f left e) list right
  | e > head list = Node left list (editThree f right e)
  | otherwise     = f (Node left list right) e

_insert :: Three a -> a -> Three a
_insert Leaf e = Node Leaf [e] Leaf
_insert (Node left list right) e = Node left (e : list) right

_min :: (Ord a) => Three a -> ([a], Three a)
_min (Node Leaf list right) = (list, right)
_min (Node left list right) =
  let (minList, newLeft) = _min left
   in (minList, Node newLeft list right)
_min _ = undefined

_rebuildNode :: (Ord a) => Three a -> Three a -> Three a
_rebuildNode Leaf right = right
_rebuildNode left Leaf = left
_rebuildNode left right =
  let (minList, newRight) = _min right
   in Node left minList newRight

_delete :: (Ord a) => Three a -> a -> Three a
_delete Leaf _ = Leaf
_delete (Node left [_] right) _ = _rebuildNode left right
_delete (Node left list right) _ = Node left (tail list) right