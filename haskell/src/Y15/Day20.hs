module Y15.Day20 (solve) where

-- Imports

import Lib.Math (isqrt)
import Lib.List (removeDups)

import Lib.Debug

import qualified Data.Map.Strict as M
import Data.List (findIndex)

-- Input Parsing 

type Input = Int

parseInput :: String -> Input
parseInput = read

-- Common

factors :: Int -> [Int]
factors n = removeDups . concat $ [[x, q] | x <- [1..isqrt(n)], let (q, r) = divMod n x, r == 0]

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = case findIndex (>= input) $ map presents [0..] of
                Nothing -> error "not found"
                Just x -> x
  where
    presents :: Int -> Int
    presents = (10 *) . sum . factors

--- Part 2

part2 :: Input -> Int
part2 input = case findIndex (>= input) $ map presents [0..] of
                Nothing -> error "not found"
                Just x -> x
  where
    presents n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
