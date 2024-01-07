module Lib.Grid where

-- Imports

import qualified Data.List as L
import qualified Data.Vector as V

import Lib.Point (Point(..))
import Lib.Graph

-- Types

-- | Grid represents a 2D grid containing a fixed-size X and Y-axis.
-- the Grid is internally represented as a 1D-Vector which maps the X and Y coordinates.
-- This was choosen instead of a 2D-Vector (e.g. V.Vector (V.Vector a)), to minimize the effort on updates.
data Grid a = Grid
  { gridPoints :: V.Vector a
  , gridBoundX :: Int
  , gridBoundY :: Int
  }
  deriving (Eq)

-- | InternalIndex maps a 2D Point described as X and Y coordinates onto a 1D axis.
type InternalIndex = Int

instance Show a => Show (Grid a) where
  show = displayShow

-- Constructors

from2DList :: [[a]] -> Grid a
from2DList ls 
  | null ls                           = error "the grid musn't be empty"
  | any (\r -> boundX /= length r) ls = error $ "all rows must have the same column length" ++ show (map length ls)
  | otherwise                         = Grid vector boundX boundY
    where
      vector = V.fromList $ concat ls
      boundY = length ls
      boundX = length $ head ls

parseFromStringWithIf :: ((Point, Char) -> a) -> String -> Grid a
parseFromStringWithIf toData string = from2DList $ zipWith (curry (\(y, l) -> zipWith (curry (toData . (\(x, c) -> (Point x y, c)))) [0..] l)) [0..] $ lines string

parseFromString :: String -> Grid Char
parseFromString = parseFromStringWithIf snd

-- Accessors

toIndex :: Grid a -> Point -> InternalIndex
toIndex g (Point x y) = y * gridBoundX g + x

fromIndex :: Grid a -> InternalIndex -> Point
fromIndex g idx = Point x y
  where
    (y, x) = divMod idx $ gridBoundX g

(!) :: Grid a -> Point -> a
g ! p = gridPoints g V.! toIndex g p

(!?) :: Grid a -> Point -> Maybe a
g !? p
  | isInBounds g p = Just $ g ! p
  | otherwise      = Nothing

xRange :: Grid a -> [Int]
xRange g = [0 .. gridBoundX g - 1]

yRange :: Grid a -> [Int]
yRange g = [0 .. gridBoundY g - 1]

xMax :: Grid a -> Int
xMax g = gridBoundX g - 1 

yMax :: Grid a -> Int
yMax g = gridBoundY g - 1 

allPoints :: Grid a -> [Point]
allPoints g = Point <$> xRange g <*> yRange g

allPointsWithValue :: Grid a -> [(Point, a)]
allPointsWithValue g = map (\p -> (p, g ! p)) $ allPoints g

corners :: Grid a -> (Point, Point, Point, Point)
corners (Grid _ x y) = (topLeft, topRight, bottomLeft, bottomRight)
  where
    topLeft     = Point 0 0
    topRight    = Point (x - 1) 0
    bottomLeft  = Point 0 (y - 1)
    bottomRight = Point (x - 1) (y - 1)

-- Checks

isInBounds :: Grid a -> Point -> Bool
isInBounds g (Point x y) = inY && inX
  where
    inY = y >= 0 && y < gridBoundY g
    inX = x >= 0 && x < gridBoundX g

-- Update

updateAt :: Point -> a -> Grid a -> Grid a
updateAt p v g@(Grid ps bx by) = Grid (ps V.// [(toIndex g p, v)]) bx by

updateAtWith :: (Point -> a -> a) -> Point -> a -> Grid a -> Grid a
updateAtWith f p v g@(Grid ps bx by) = Grid (ps V.// [(toIndex g p, f p v)]) bx by

(//) :: Grid a -> [(Point, a)] -> Grid a
g@(Grid ps bx by) // pvs = Grid (ps V.// map (\(p, v) -> (toIndex g p, v)) pvs) bx by

-- Display

displayWith :: (Point -> a -> String) -> Grid a -> String
displayWith f g = L.intercalate "" [ let p = Point x y 
                                         s = if x == xMax g then "\n" else "" 
                                      in f p (g ! p) ++ s | y <- yRange g, x <- xRange g 
                                   ]

display :: Grid Char -> String
display = displayWith (\_ c -> [c])

displayStr :: Grid String -> String
displayStr = displayWith (\_ c -> c)

displayShow :: Show a => Grid a -> String
displayShow = displayWith (\_ c -> show c)
