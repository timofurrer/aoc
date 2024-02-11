module Y15.Day07 (solve) where

-- Imports

import qualified Data.Map as M
import Lib.Parsing

-- Input Parsing 

type Input = [Int]

type Instruction = WireSignals -> WireSignals

type WireSignals = M.Map String Int

parseInput :: String -> Input
-- parseInput = parseLinesWith instruction
parseInput _ = []

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
