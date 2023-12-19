module Lib.List (safeIndex, consecutiveDiff) where

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i 
        | (i > -1) && (length xs > i) = Just (xs !! i)
        | otherwise = Nothing

consecutiveDiff :: Num a => [a] -> [a]
consecutiveDiff [] = []
consecutiveDiff ls = zipWith (-) (tail ls) ls
