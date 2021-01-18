{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Hw4.Task8
  ( -- * Functions
    evolve
  , gridWithOneIl
  )
where

import Control.Comonad (Comonad (..))
import Control.Lens (each, filtered, makeLenses, (&), (.~), (^.), (^..))
import Data.List (intercalate)
import System.Random (Random, RandomGen, StdGen, mkStdGen, randomR, randoms, split)

data ListZipper a = LZ [a] a [a]
  deriving (Functor)

newtype Grid a = Grid {unGrid :: ListZipper (ListZipper a)}
  deriving (Functor)

-- | The tuple of time (incubation, symptom, immunity)
type IlTime =
  (Integer, Integer, Integer)

-- | Possible conditions of the illness
data IlState
  = Healthy
  | Incubation
  | Symptom
  | Immunity

-- | Description of the patient
data Rat a = Rat
  { _currentTime :: Integer,  -- ^ Time to change to a new state
    _currentState :: IlState, -- ^ Current state of the illness
    _gen :: a                 -- ^ Generator for calculating probability
  }

makeLenses ''Rat

-- | Describes the state of the game: time, current state of
-- @Rar@, probability of infection
data CovidGrid a
  =  (Floating a, Ord a, Random a)
  => CovidGrid (Grid (Rat StdGen)) IlTime (Int, Int) a

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

instance Show IlState where
  show :: IlState -> String
  show Healthy    = " "
  show Incubation = "."
  show Symptom    = "#"
  show Immunity   = "@"

instance Show (CovidGrid a) where
  show :: CovidGrid a -> String
  show (CovidGrid grid _ (w, h) _) =
    let charGrid = fmap (\x -> x ^. currentState & show) grid
     in "|"
        ++ intercalate "|\n|"
          (toList (fmap (\x -> concat (toList x w)) (unGrid charGrid)) h)
        ++ "|"

-- | Initializes a @CovidGrid@ with one infected in the center
gridWithOneIl
  :: (Floating a, Ord a, Random a)
  => Int                  -- ^ Seed for generator
  -> IlTime               -- ^ The tuple of time (incubation, symptom, immunity)
  -> (Int, Int)           -- ^ (Height, width) of the generated @Grid@
  -> a                    -- ^ Given probability
  -> CovidGrid a
gridWithOneIl seed time = CovidGrid (makeGrid (mkStdGen seed) time) time

-- | Performing a single simulation step
evolve
  :: CovidGrid a          -- ^ Input @CovidGrid@
  -> CovidGrid a          -- ^ The state after executing one step of simulation
evolve (CovidGrid grid time size prob) =
  CovidGrid (extend (gameCovid prob time) grid) time size prob

----------------------------------------------
--    auxiliary functions (CovidGame)      ---
----------------------------------------------

-- | Returns `True` if it is healthy,
-- `False` otherwise
isHelthy :: IlState -> Bool
isHelthy Healthy = True
isHelthy Immunity = True
isHelthy _ = False

-- | Returns `True` if it is ill,
-- `False` otherwise
isIll :: IlState -> Bool
isIll = not . isHelthy

-- | Counts the number of infected @Rat@
illCount :: [Rat a] -> Int
illCount rats = length (rats ^.. each . currentState . filtered isIll)

-- | Returns the current neighbors
neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals = [left, right]
    verticals = [up, down]

-- | Counts the number of infected neighbors
illNeighbours :: Grid (Rat a) -> Int
illNeighbours g =
  illCount $
    map (\direction -> extract $ direction g) neighbours

-- | Simulates the bite and transition of states
-- (also decreases the time in the current state)
ratBite
  :: Bool                 -- ^ True if an infection has been detected
  -> IlTime               -- ^ The tuple of time (incubation, symptom, immunity)
  -> Rat a                -- ^ Current status
  -> Rat a                -- ^ New status
ratBite _ (_, s, _) (Rat 0 Incubation g) = Rat s Symptom g
ratBite _ (_, _, i) (Rat 0 Symptom g) = Rat i Immunity g
ratBite _ _ (Rat 0 Immunity g) = Rat 0 Healthy g
ratBite True (i, _, _) (Rat _ Healthy g) = Rat i Incubation g
ratBite _ _ (Rat x s g) = Rat (x - 1) s g

-- | Converts a focused rat to a new state, taking
-- into account the possibility of infection (by Covid).
gameCovid
  :: (Floating a, Ord a, Random a, RandomGen g)
  => a                    -- ^ Given probability
  -> IlTime               -- ^ The tuple of time (incubation, symptom, immunity)
  -> Grid (Rat g)         -- ^ Grid with @Rat@
  -> Rat g                -- ^ New state of the focused @Rat@
gameCovid prob t grid =
  let x = extract grid
      (genProb, newGen) = randomR (0, 1) (x ^. gen)
      bite = prob > (genProb ^ illNeighbours grid)
   in ratBite bite t x & gen .~ newGen

-- | Generates a ListZiper with random @Int@ values
makeListZipInt
  :: (RandomGen g)
  => g                    -- ^ Given generator
  -> ListZipper Int       -- ^ ListZiper with random @Int@ values
makeListZipInt generator =
  let (gen1, gen2) = split generator
   in LZ
        (randoms gen1)
        (head (randoms generator))
        (randoms gen2)

-- | Generates a ListZiper with ListZiper with random @Int@ values
makeListZipInt'
  :: (RandomGen g)
  => g                    -- ^ Given generator
  -> ListZipper (ListZipper Int)
makeListZipInt' generator =
  fmap
    (makeListZipInt . mkStdGen)
    (makeListZipInt generator)

-- | Initializes a @Grid@ of @Rat@
-- with one infected in the center
makeGrid
  :: (RandomGen g)
  => g                    -- ^ Given generator
  -> IlTime               -- ^ The tuple of time (incubation, symptom, immunity)
  -> Grid (Rat StdGen)
makeGrid generator (i, _, _) =
  let lz = makeListZipInt' generator
   in gridWrite
        (Rat i Incubation (mkStdGen 0))
        (fmap (Rat 0 Healthy . mkStdGen) (Grid lz))

----------------------------------------------
--      auxiliary functions (ListZipper)   ---
----------------------------------------------

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft _ = error "listLeft"
listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

genericMove ::
  (z a -> z a) ->
  (z a -> z a) ->
  z a ->
  ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

----------------------------------------------
--         auxiliary functions (Grid)      ---
----------------------------------------------

up, down :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical = genericMove up down
