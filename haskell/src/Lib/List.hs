module Lib.List (counter, minimumsBy, lastN, pop, safeIndex, consecutiveDiff, removeDups) where

import Data.Function (on)
import Data.List (sort, group, sortBy, groupBy)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i 
        | (i > -1) && (length xs > i) = Just (xs !! i)
        | otherwise = Nothing

consecutiveDiff :: Num a => [a] -> [a]
consecutiveDiff [] = []
consecutiveDiff ls = zipWith (-) (tail ls) ls

pop :: [a] -> (a, [a])
pop [] = error "list mustn't be empty to pop first element"
pop (x : xs) = (x, xs)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs -n) xs

minimumsBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
minimumsBy f = groupBy (\a b -> f a == f b) . sortBy (compare `on` f)

counter :: Ord a => [a] -> [(a, Int)]
counter = map (\x -> (head x, length x)) . group . sort

-- | Removes duplications from a list, with O(N log N) instead of O(N^2) (like nub)
removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . group . sort
