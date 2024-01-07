module Lib.List (minimumsBy, lastN, pop, safeIndex, consecutiveDiff) where

import Data.Function (on)
import Data.List (sortBy, groupBy)

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
