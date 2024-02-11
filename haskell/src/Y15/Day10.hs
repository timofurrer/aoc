module Y15.Day10 (solve) where

-- Imports

import Lib.Debug

import Data.List (group)

-- Input Parsing 

type Input = String

parseInput :: String -> Input
parseInput = id

-- Common

loudSeq :: String -> String
loudSeq = concatMap (\x -> show (length x) ++ [head x]) . group

run :: [a] -> String -> String
run = flip (foldl (\acc _ -> loudSeq acc))

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = length . run [0 :: Int ..39]

--- Part 2

part2 :: Input -> Int
part2 = length . run [0 :: Int ..49]

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
