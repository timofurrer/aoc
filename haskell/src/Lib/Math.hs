module Lib.Math (isqrt) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
