module Y15.Day04 (solve) where

-- Imports

import Crypto.Hash (hash, Digest, MD5)
import Data.ByteString.Char8 (pack)
import qualified Data.List as L

-- Input Parsing 

type Input = String

parseInput :: String -> Input
parseInput x = x

-- Common

md5 :: String -> Digest MD5
md5 = hash . pack

md5sum :: String -> String
md5sum = show . md5

hasLeadingZeros :: Int -> [Char] -> Bool
hasLeadingZeros n = L.isPrefixOf (replicate n '0')

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = go 1
  where
    go n
      | hasLeadingZeros 5 . md5sum $ input ++ show n = n
      | otherwise                                      = go (n + 1)

--- Part 2

part2 :: Input -> Int
part2 input = go 1
  where
    go n
      | hasLeadingZeros 6 . md5sum $ input ++ show n = n
      | otherwise                                    = go (n + 1)

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
