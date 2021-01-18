{-# LANGUAGE InstanceSigs #-}

module Hw1.Part1.Task1
  ( -- * `WeeksDay` constructor
    WeeksDay(..)
    
    -- * Functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

data WeeksDay
  = Saturday    -- ^ Saturday
  | Sunday      -- ^ Sunday
  | Monday      -- ^ Monday
  | Tuesday     -- ^ Tuesday
  | Wednesday   -- ^ Wednesday
  | Thursday    -- ^ Thursday
  | Friday      -- ^ Friday
  deriving Show -- ^ default 'Show' instance

-- | Returns the constructor for the `WeeksDay` after the given `WeeksDay`.
nextDay
  :: WeeksDay -- ^ given `WeeksDay`
  -> WeeksDay -- ^ next `WeeksDay`
nextDay = succ

-- | Returns the constructor for the `WeeksDay` after @x@ days of a given `WeeksDay`.
afterDays
  :: WeeksDay -- ^ given `WeeksDay`
  -> Int      -- ^ number of days after
  -> WeeksDay -- ^ `WeeksDay` after @x@ days of a given `WeeksDay`
afterDays day 0 = day
afterDays day x = afterDays (nextDay day) (x - 1)

-- | Determines if the day is a weekend.
isWeekend
  :: WeeksDay -- ^ given `WeeksDay`
  -> Bool     -- ^ @True@ if given `WeeksDay` is a weekend
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Determines the number of days from a given `WeeksDay` to @Friday@
daysToParty
  :: WeeksDay -- ^ given `WeeksDay`
  -> Int      -- ^ the number of days from a given `WeeksDay` to @Friday@
daysToParty Friday = 0
daysToParty day = daysToParty (nextDay day) + 1

-- | customize 'Enum' instance
instance Enum WeeksDay where
  toEnum :: Int -> WeeksDay
  toEnum x
    = [ Saturday, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday ]
      !! (x `mod` 7)

  fromEnum :: WeeksDay -> Int
  fromEnum Saturday  = 0
  fromEnum Sunday    = 1
  fromEnum Monday    = 2
  fromEnum Tuesday   = 3
  fromEnum Wednesday = 4
  fromEnum Thursday  = 5
  fromEnum Friday    = 6

-- | customize 'Eq' instance
instance Eq WeeksDay where
  (==) :: WeeksDay -> WeeksDay -> Bool
  x == y = fromEnum x == fromEnum y