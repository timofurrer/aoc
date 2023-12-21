module Lib.Grid where

-- Imports

import qualified Data.List as L
import qualified Data.Vector as V

import Lib.Point (Point(..))

-- Types

data Grid a = Grid
  { gridPoints :: V.Vector (V.Vector a)
  , gridBoundX :: Int
  , gridBoundY :: Int
  }
  deriving (Eq)

--instance Show Char => Show (Grid a) where
--  show g = displayWith (\_ c -> [c]) g


-- Constructors

from2DVector :: V.Vector (V.Vector a) -> Grid a
from2DVector v 
  | V.length v == 0                      = error "the grid musn't be empty"
  | V.any (\r -> boundX /= V.length r) v = error $ "all rows must have the same column length" ++ show (V.map V.length v)
  | otherwise                            = Grid v boundX boundY
    where
      boundY = V.length v
      boundX = V.length (v V.! 0)

parseFromStringWithIf :: ((Point, Char) -> a) -> String -> Grid a
parseFromStringWithIf toData string = from2DVector $ V.fromList $ map (\(y, l) -> V.fromList $ map toData $ map (\(x, c) -> (Point x y, c)) $ zip [0..] l) $ zip [0..] $ lines string

parseFromString :: String -> Grid Char
parseFromString = parseFromStringWithIf snd

-- Accessors

(!) :: Grid a -> Point -> a
g ! p = (gridPoints g V.! py p) V.! px p

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

--updateAt :: Point -> a -> Grid a -> Grid a
--updateAt p v g = 

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
