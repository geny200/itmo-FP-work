{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Hw4.Task1
  ( -- * Constructor Point
    Point (..)

    -- * Functions
  , crossProduct
  , doubleArea
  , distance
  , minus
  , perimeter
  , plus
  , scalarProduct
  )
where

import Control.Lens (makeLenses, (%~), (&), (^.))

-- | Datatype for representing a point in a
-- two-dimensional plane with integer coordinates.
data Point = Point
  { _x :: Integer,  -- ^ the integer X coordinate.
    _y :: Integer   -- ^ the integer Y coordinate.
  }

makeLenses ''Point

-- | Adds the coordinates of points `(x1 + x2, y1 + y2)`
plus :: Point -> Point -> Point
plus a b = a & x %~ (+ b ^. x) & y %~ (+ b ^. y)

-- | Subtracts the coordinates of points `(x1 - x2, y1 - y2)`
minus :: Point -> Point -> Point
minus a b = a & x %~ (+ negate (b ^. x)) & y %~ (+ negate (b ^. y))

-- | Calculates the scalar product (x1 * x2 + y1 * y2)
scalarProduct :: Point -> Point -> Integer
scalarProduct a b = a ^. x * b ^. x + a ^. y * b ^. y

-- | Calculates the cross product (x1 * y2 - x2 * y1)
crossProduct :: Point -> Point -> Integer
crossProduct a b = a ^. x * b ^. y - b ^. x * a ^. y

-- | Calculates the distance between points
distance :: Point -> Point -> Double
distance a b =
  let dist = minus a b
   in sqrt . fromIntegral $ scalarProduct dist dist

-- | Performs a quick convolution with the current action.
myZipWith :: (Num b) => (a -> a -> b) -> [a] -> b
myZipWith action points = myZip points 0
  where
    myZip [] !acc = acc
    myZip [a] !acc = action a (head points) + acc
    myZip (a : b : bs) !acc = myZip (b : bs) (action a b + acc)

-- | Calculates the perimeter of figure
perimeter :: [Point] -> Double
perimeter = myZipWith distance

-- | Calculates the double Area of figure
doubleArea :: [Point] -> Integer
doubleArea = myZipWith crossProduct
