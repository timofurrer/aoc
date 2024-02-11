-- module Y15.Day08 (solve) where
module Y15.Day08 where

-- Imports

import Data.List (dropWhileEnd)

-- Input Parsing

type Input = [String]

parseInput :: String -> Input
parseInput = lines

-- Common

unescapedLength :: String -> Int
unescapedLength [] = 0
unescapedLength ('\\' : 'x' : _ : _ : xs) = 1 + unescapedLength xs
unescapedLength ('\\' : '\\' : xs) = 1 + unescapedLength xs
unescapedLength ('\\' : '"' : xs) = 1 + unescapedLength xs
unescapedLength (_ : xs) = 1 + unescapedLength xs

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = sum . map len
  where
    len s = length s - unescapedLength (removeSurroundingQuotes s)
    removeSurroundingQuotes = tail . init


--- Part 2

escapedLength :: String -> Int
escapedLength [] = 0
escapedLength ('"' : xs) = 2 + escapedLength xs
escapedLength ('\\' : '\\' : xs) = 4 + escapedLength xs
escapedLength ('\\' : '"' : xs) = 4 + escapedLength xs 
escapedLength ('\\' : 'x' : _ : _ : xs) = 5 + escapedLength xs
escapedLength (_ : xs) = 1 + escapedLength xs

part2 :: Input -> Int
part2 = sum . map len
  where
    len s = 2 + escapedLength s - length s

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
