module Y15.Day15 where

-- Imports

import Lib.Debug

import Lib.Parsing

import Data.List (transpose)

-- Input Parsing 

type Input = [Ingredient]

type Ingredient = [Int]

parseInput :: String -> Input
parseInput = parseLinesWith ingredient
  where
    ingredient = do
      _ <- word
      _ <- symbol ": capacity"
      cap <- number
      _ <- symbol ", durability"
      dur <- number
      _ <- symbol ", flavor"
      fla <- number
      _ <- symbol ", texture"
      tex <- number
      _ <- symbol ", calories"
      cal <- number
      pure [cap, dur, fla, tex, cal]

-- Common

totalTeaspoons :: Int
totalTeaspoons = 100

-- Solutions

score :: Input -> [Int] -> Int
score is c = product $ map (max 0 . sum) $ transpose $ zipWith (\a bs -> map (a *) bs) c is

combinations :: Input -> [[Int]]
combinations input = [ x | x <- sequence combs, sum x == totalTeaspoons ]
  where
    combs = [ [1..totalTeaspoons - length input + 2] | _ <- [1..length input]]


--- Part 1

part1 :: Input -> Int
part1 input = maximum . map (score inputWithCal) $ combinations input
  where
    inputWithCal = map init input

--- Part 2

scoreWithCal :: Input -> [Int] -> (Int, Int)
scoreWithCal is c = apply . map (max 0 . sum) $ transpose $ zipWith (\a bs -> map (a *) bs) c is
  where
    apply x = (product $ init x, last x)

part2 :: Input -> Int
part2 input = maximum . map fst . filter (\(_, c) -> c == 500) . map (scoreWithCal input) $ combinations input

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
