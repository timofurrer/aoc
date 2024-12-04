module Y15.Day22 (solve) where

-- Imports

import Lib.Parsing

-- Input Parsing 

type Input = Boss

type Boss = (Int, Int) -- (hp, damage)

parseInput :: String -> Input
parseInput = parseWith boss
  where
    boss = do
      _ <- symbol "Hit Points:"
      hp <- number
      _ <- symbol "Damage:"
      da <- number
      pure (hp, da)

type Me = (Int, Int, Int, Int) -- (Mana, HP, damage, armor)

type Effect = (Int, Int, Int, Int)-- (turns, mana, damage, armor)

-- Common

magicMissile :: Me -> Boss -> [Effect]

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
