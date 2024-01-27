module Y18.DayXX (solve) where

-- Imports

import Lib.Debug

-- Input Parsing 

type Input = [String]

parseInput :: String -> Input
parseInput = lines

-- Common

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = 42

--- Part 2
          
part2 :: Input -> Int
part2 input = 42

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
