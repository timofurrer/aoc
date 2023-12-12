module Lib.Geometry where

import Data.Complex

type Point2d = Complex Integer

getX :: Point2d -> Integer
getX = realPart

getY :: Point2d -> Integer
getY = imagPart
