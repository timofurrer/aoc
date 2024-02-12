module Y15.Day11 where

-- Imports

import Data.List (group)
import Data.Char (ord, chr)

-- Input Parsing 

type Input = [Int]

parseInput :: String -> Input
parseInput = map ord

-- Common

-- Solutions

straightOfThree :: [Int] -> Bool
straightOfThree (a : b : c : xs) = a + 1 == b && b + 1 == c || straightOfThree (b : c : xs)
straightOfThree _ = False

allValid :: [Int] -> Bool
allValid = not . any (\x -> isi x || iso x || isl x)
  where
    isi = (==) (ord 'i')
    iso = (==) (ord 'o')
    isl = (==) (ord 'l')

twoPairs :: [Int] -> Bool
twoPairs xs = 1 < length (filter (== 2) $ map length $ group xs)

isValidPassword :: [Int] -> Bool
isValidPassword xs = straightOfThree xs && allValid xs && twoPairs xs

increment :: [Int] -> [Int]
increment = reverse . go . reverse
  where
    go :: [Int] -> [Int]
    go [] = []
    go (x : xs)
      | x == ord 'z' = ord 'a' : go xs
      | otherwise    = (x + 1) : xs

findNext :: [Int] -> [Int]
findNext xs
  | isValidPassword xs = xs
  | otherwise          = findNext $ increment xs

--- Part 1

part1 :: Input -> String
part1 = map chr . findNext

--- Part 2

part2 :: Input -> String
part2 = map chr . findNext . increment . findNext

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
