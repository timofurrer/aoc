module AOC.Day03 (solve) where

import Data.List.Split (splitOn)
import qualified Data.Map as MP

solve :: String -> (String, String)
solve input = 
  (show $ part1 input, 
   show $ part2 input
  )

parse :: String -> [(Int, ((Int, Int), (Int, Int)))]
parse = zip [1..] . map parseLine . lines

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = (x, y)
  where 
    s = splitOn ":" . last $ splitOn "@" line
    x = toTuple2 $ map read . splitOn "," $ head s
    y = toTuple2 $ map read . splitOn "x" $ last s

toTuple2 :: [a] -> (a, a)
toTuple2 [x, y] = (x, y)
toTuple2 _ = error "must have two items"

part1 :: String -> Int
part1 input = length $ MP.filter (>= 2) $ fabrics MP.empty $ parse input

fabrics :: MP.Map (Int, Int) Int -> [(Int, ((Int, Int), (Int, Int)))] -> MP.Map (Int, Int) Int
fabrics m [] = m
fabrics m ((_, ((x, y), (w, h))) : xs) = 
  let
    ps = [ (x', y') | x' <- [x..x + w - 1], y' <- [y..y + h - 1] ]

    go [] m' = m'
    go (k : xs') m' = go xs' (MP.insertWith (+) k 1 m')

  in fabrics (go ps m) xs


part2 :: String -> Int
part2 input = go $ parse input
  where
    f = fabrics MP.empty $ parse input

    go [] = 0
    go ((c, ((x, y), (w, h))) : xs) = if all (== 1) [ MP.findWithDefault 1 (x', y') f | x' <- [x..x + w - 1], y' <- [y..y + h - 1] ] 
      then c
      else go xs
