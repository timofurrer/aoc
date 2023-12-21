module Lib.Point where

-- Imports

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- Types

-- | Point in a 2D plane, described by an X and Y coordinate.
data Point = Point
  { px :: !Int,
    py :: !Int
  }
  deriving (Eq, Generic, Ord)

instance Show Point where
  show (Point x y) = printf "(%d,%d)" x y

-- | Point must be hashable so that it can be stored in the HashMap-Grid 
-- A generic hashing is used for Point, which is derived from Generic.
instance Hashable Point where


-- | Vector in a 2D plane, described by an X and Y coordinate.
type Vector = Point

-- Modification

(.+) :: Point -> Vector -> Point
p .+ v = add p v

add :: Point -> Vector -> Point
add (Point x y) (Point dx dy) = Point (x + dx) (y + dy)

-- Point Neighbors

aboveOf :: Point -> Point 
aboveOf (Point x y) = Point x (y - 1)

belowOf :: Point -> Point 
belowOf (Point x y) = Point x (y + 1)

rightOf :: Point -> Point 
rightOf (Point x y) = Point (x + 1) y

leftOf :: Point -> Point 
leftOf (Point x y) = Point (x - 1) y

fourNeighborPoints :: Point -> [Point]
fourNeighborPoints p = [aboveOf p, rightOf p, belowOf p, leftOf p]

-- Distances

euclidianDistance :: Point -> Point -> Double
euclidianDistance (Point x1 y1) (Point x2 y2) = let dx = x1 - x2
                                                    dy = y1 - y2
                                                 in sqrt $ fromIntegral $ dx ^ 2 + dy ^ 2

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x1 y1) (Point x2 y2) = let dx = x1 - x2
                                                    dy = y1 - y2
                                                 in abs dx - abs dy
