module Lib.Combinations where

import qualified Data.List as L

-- | combinations produces a 2-tuple list of combinations from the given list, 
-- which excludes duplicates.
combinations :: [a] -> [(a, a)]
combinations xs = [ (x, y) | (x : rest) <- L.tails xs, y <- rest ]
