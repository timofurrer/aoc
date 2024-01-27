module AOC.Day14 (solve) where

-- Imports

import Lib.Debug

import qualified Data.List as L
import qualified Data.Digits as D
import qualified Data.Sequence as S
import qualified Data.Foldable as F

-- Input Parsing 

type Input = Int

parse :: String -> Input
parse = read

-- Common

type Scoreboard = S.Seq Int

type Elves = [Int]

pairToSeq :: (Int, Int) -> S.Seq Int
pairToSeq (a, b) = S.fromList [a, b]

brew :: Int -> Elves -> Scoreboard -> Int
brew n elves scoreboard
  | S.length scoreboard >= n = D.unDigits 10 $ F.toList $ S.take 10 $ S.drop (n-10) scoreboard
  | otherwise                = let s = foldl (\acc e -> acc + S.index scoreboard e) 0 elves
                                   scoreboard' = if s >= 10 then scoreboard S.>< pairToSeq (s `divMod` 10) else scoreboard S.|> s
                                   elves' = map (\e -> (e + 1 + S.index scoreboard' e) `mod` S.length scoreboard') elves
                                in brew n elves' scoreboard'

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = brew (input + 10) [0, 1] $ S.fromList [3, 7]

--- Part 2
          
part2 :: Input -> Int
part2 input = brew2 (D.digits 10 input) [0, 1] $ S.fromList [3, 7]

brew2 :: [Int] -> Elves -> Scoreboard -> Int
brew2 goal elves scoreboard
  | goal `L.isPrefixOf` lastScores = S.length scoreboard - length goal - 2
  | goal `L.isSuffixOf` lastScores = S.length scoreboard - length goal
  | goal `L.isInfixOf` lastScores = S.length scoreboard - length goal - 1
  | otherwise                     = let s = foldl (\acc e -> acc + S.index scoreboard e) 0 elves
                                        scoreboard' = if s >= 10 then scoreboard S.>< pairToSeq (s `divMod` 10) else scoreboard S.|> s
                                        elves' = map (\e -> (e + 1 + S.index scoreboard' e) `mod` S.length scoreboard') elves
                                     in brew2 goal elves' scoreboard'
    where
      lastScores = F.toList $ takeLastN (length goal + 2) scoreboard

takeLastN :: Int -> S.Seq a -> S.Seq a
takeLastN n s = S.drop (S.length s - n) s

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parse rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
