module Lib.SparseGrid where

-- Imports

import qualified Data.List as L
import qualified Data.HashMap as H

import Lib.Point

-- Types

type Grid = H.Map Point

-- Constructors

parseFromStringWithIf :: String -> ((Point, Char) -> Bool) -> ((Point, Char) -> (Point, a)) -> Grid a
parseFromStringWithIf string isPoint toData = H.fromList $ L.concatMap (\(y, l) -> map toData . filter isPoint $ parseRow y l) $ zip [0..] $ lines string
  where 
    parseRow :: Int -> [Char] -> [(Point, Char)]
    parseRow y l = map (\(x, c) -> (Point x y, c)) $ zip [0..] l

