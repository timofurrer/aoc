module AOC.Day13 (solve) where

-- Imports

import Lib.Debug

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

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parse rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
