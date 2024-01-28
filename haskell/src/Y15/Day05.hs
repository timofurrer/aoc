module Y15.Day05 (solve) where

-- Imports

import qualified Data.Map as M
import qualified Data.List as L

import Lib.List (counter)
import Data.List (isInfixOf)

-- Input Parsing 

type Input = [String]

parseInput :: String -> Input
parseInput = lines

-- Common

-- Solutions

--- Part 1

threeVowels :: [Char] -> Bool
threeVowels s = sum (map v "aeiou") >= 3
  where
    cs :: M.Map Char Int
    cs = M.fromList $ counter s
    v :: Char -> Int
    v c = M.findWithDefault 0 c cs

twiceInARow :: Eq a => [a] -> Bool
twiceInARow s = any (\c -> length c >= 2) $ L.group s

containsNotBad :: [Char] -> Bool
containsNotBad s = all (\p -> not $ p `isInfixOf` s) ["ab", "cd", "pq", "xy"]

part1 :: Input -> Int
part1 = length . filter threeVowels . filter twiceInARow . filter containsNotBad

--- Part 2

hasPairs :: Eq a => [a] -> Bool
hasPairs (s1 : s2 : ss)
  | [s1, s2] `isInfixOf` ss = True
  | otherwise               = hasPairs (s2 : ss)
hasPairs _ = False

oneInBetween :: Eq a => [a] -> Bool
oneInBetween (s1 : x : s2 : ss) 
  | s1 == s2  = True
  | otherwise = oneInBetween (x : s2 : ss)
oneInBetween _ = False

part2 :: Input -> Int
part2 = length . filter hasPairs . filter oneInBetween

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
