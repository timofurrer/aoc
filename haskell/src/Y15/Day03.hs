module Y15.Day03 (solve) where

-- Imports

import qualified Data.Set as S

import Lib.Point

-- Input Parsing 

type Input = [Direction]

parseInput :: String -> Input
parseInput = map dirs
  where
    dirs '^' = N
    dirs '>' = E
    dirs 'v' = S
    dirs '<' = W
    dirs _   = error "invalid input"

-- Common

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = S.size . go (S.fromList [unit]) unit
  where
    go s _ []       = s
    go s p (v : vs) = let p' = p --> v in go (S.insert p' s) p' vs

--- Part 2

splitForSanta :: [a] -> [(a, a)]
splitForSanta [] = []
splitForSanta (x : y : xs) = (x, y) : splitForSanta xs
splitForSanta _ = error "invalid input"

part2 :: Input -> Int
part2 input = S.size . go (S.fromList [unit]) unit unit $ splitForSanta input
  where
    go s _ _ []                = s
    go s p1 p2 ((v1, v2) : vs) = let p1' = p1 --> v1
                                     p2' = p2 --> v2
                                  in go (S.insert p1' (S.insert p2' s)) p1' p2' vs

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
