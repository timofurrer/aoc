module AOC.DayXX (solve) where

-- Imports

import Lib.Debug

-- Main

solve :: String -> (String, String)
solve input = 
  (show $ part1 $ parse input, 
   show $ part2 $ parse input
  )

-- Input Parsing 

type Input = [String]

parse :: String -> Input
parse = lines

-- Part 1

part1 :: Input -> Int
part1 input = 42

-- Part 2
          
part2 :: Input -> Int
part2 input = 42
