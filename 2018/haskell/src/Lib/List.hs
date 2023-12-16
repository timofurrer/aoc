module Lib.List (safeIndex) where

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i 
        | (i > -1) && (length xs > i) = Just (xs !! i)
        | otherwise = Nothing
