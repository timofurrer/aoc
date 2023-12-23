module Lib.List (pop, safeIndex, consecutiveDiff) where

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
