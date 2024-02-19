module Y15.Day17 (solve) where

-- Imports

import Lib.List (minimumsBy)
import Lib.Combinations (combinations)

-- Input Parsing 

type Input = [Volume]

type Volume = Int

parseInput :: String -> Input
parseInput = map (\x -> read x :: Volume) . lines

targetVolume :: Volume
targetVolume = 150

-- Common

fillToTarget :: Volume -> [Volume] -> [[Volume]]
fillToTarget target vs = filter (\x -> sum x == target) $ foldl (\acc x -> acc ++ combinations x vs) [] [1..length vs]

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = length . fillToTarget targetVolume

--- Part 2

part2 :: Input -> Int
part2 = length . head . minimumsBy length . fillToTarget targetVolume

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
