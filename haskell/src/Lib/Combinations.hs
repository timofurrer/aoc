module Lib.Combinations where

import qualified Data.List as L

-- | combinations2 produces a 2-tuple list of combinations from the given list, 
-- which excludes duplicates.
combinations2 :: [a] -> [(a, a)]
combinations2 xs = [ (x, y) | (x : rest) <- L.tails xs, y <- rest ]

-- | combinations produces a k-length list of combinations from the given list.
combinations :: Int -> [a] -> [[a]]
combinations k = filter ((k==) . length) . L.subsequences

-- | Use sequence for N-cartesian product
