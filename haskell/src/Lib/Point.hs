module Lib.Point where

-- Imports

import Data.Hashable (Hashable)
import GHC.Generics (Generic, M1 (M1))
import Text.Printf (printf)

-- Types

-- | Point in a 2D plane, described by an X and Y coordinate.
data Point = Point
  { px :: !Int,
    py :: !Int
  }
  deriving (Eq, Generic)

instance Show Point where
  show (Point x y) = printf "(%d,%d)" x y

instance Ord Point where
  -- Comparison happens in reading order -> that is, top-to-bottom, left-to-right.
  (Point x1 y1) `compare` (Point x2 y2) = let c = y1 `compare` y2
                                           in if c == EQ
                                                then x1 `compare` x2
                                                else c

-- | Point must be hashable so that it can be stored in the HashMap-Grid 
-- A generic hashing is used for Point, which is derived from Generic.
instance Hashable Point where


-- | Vector in a 2D plane, described by an X and Y coordinate.
type Vector = Point

-- Constructors

unit :: Point
unit = Point 0 0

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

northEastOf :: Point -> Point
northEastOf (Point x y) = Point (x + 1) (y - 1)

southEastOf :: Point -> Point
southEastOf (Point x y) = Point (x + 1) (y + 1)

southWestOf :: Point -> Point
southWestOf (Point x y) = Point (x - 1) (y + 1)

northWestOf :: Point -> Point
northWestOf (Point x y) = Point (x - 1) (y - 1)

eightNeighborPoints :: Point -> [Point]
eightNeighborPoints p = [aboveOf p, northEastOf p, rightOf p, southEastOf p, belowOf p, southWestOf p, leftOf p, northWestOf p]

-- Directions and Turns

data Direction 
  = N 
  | E 
  | S 
  | W
  deriving (Show, Eq)

directionToVector :: Direction -> Vector
directionToVector N = Point 0 (-1)
directionToVector E = Point 1 0
directionToVector S = Point 0 1
directionToVector W = Point (-1) 0

parseDirection :: Char -> Direction
parseDirection 'N' = N
parseDirection 'E' = E
parseDirection 'S' = S
parseDirection 'W' = W
parseDirection x   = error ("invalid direction '" ++ show x ++ "'")

(-->) :: Point -> Direction -> Point
p --> d = p .+ directionToVector d

turnClockwise90 :: Direction -> Direction
turnClockwise90 N = E
turnClockwise90 E = S
turnClockwise90 S = W
turnClockwise90 W = N

turnCounterClockwise90 :: Direction -> Direction
turnCounterClockwise90 N = W
turnCounterClockwise90 W = S
turnCounterClockwise90 S = E
turnCounterClockwise90 E = N
  

-- Distances

euclidianDistance :: Point -> Point -> Double
euclidianDistance (Point x1 y1) (Point x2 y2) = let dx = x1 - x2
                                                    dy = y1 - y2
                                                 in sqrt $ fromIntegral $ dx ^ 2 + dy ^ 2

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x1 y1) (Point x2 y2) = let dx = x1 - x2
                                                    dy = y1 - y2
                                                 in abs dx - abs dy

-- Misc

pointsInRectangle :: (Int, Int) -> (Int, Int) -> [Point]
pointsInRectangle (f1, f2) (t1, t2) = [ Point x y | x <- [f1..t1], y <- [f2..t2] ]
