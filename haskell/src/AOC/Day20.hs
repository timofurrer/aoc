module AOC.Day20 (solve) where

-- Imports

import Lib.Debug

import Lib.Point

import Data.Map.Strict as Map

-- Input Parsing 

type Input = [Char]

parseInput :: String -> Input
parseInput input = input

-- Common

-- buildMap :: [Char] -> Map.Map Point [Point]
-- buildMap regex = _
--   where
--     go queue 

expandRegex :: [Char] -> [String]
expandRegex regex = go "" regex
  where
    go :: String -> [Char] -> [String]
    go exp []       = []
    go exp (r : rs) 
      | r == '^' = go "" rs -- we are just starting, could actually do that directly in expandRegex
      | r == '$' = exp

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
