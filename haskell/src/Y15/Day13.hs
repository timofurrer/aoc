module Y15.Day13 (solve) where

-- Imports

import Lib.Parsing
import Lib.List (removeDups)

import Text.Parsec
import Data.List (permutations)
import qualified Data.Map as M

-- Input Parsing 

type Input = [Arrangement]

type Arrangement = ((Person, Person), Int)

type Person = String

parseInput :: String -> Input
parseInput = parseLinesWith arrangement
  where
    arrangement = do
      p1 <- word
      _ <- symbol "would"
      gl <- symbol "gain" <|> symbol "lose"
      n <- number
      _ <- symbol "happiness units by sitting next to"
      p2 <- word

      pure ((p1, p2), happinessScore n gl)

    happinessScore n "gain" = n
    happinessScore n "lose" = n * (-1)
    happinessScore _ _ = error "invalid input"

-- Common

persons :: [Arrangement] -> [Person]
persons = removeDups . concatMap (\((p1, p2), _) -> [p1, p2])

seatingOrders :: [Person] -> [[Person]]
seatingOrders = permutations

happiness :: [Arrangement] -> [Person] -> Int
happiness as ps = snd $ foldl (\(x', h) x -> (x, h + (hs M.! (x', x)) + (hs M.! (x, x')))) (last ps, 0) ps
  where
    hs :: M.Map (Person, Person) Int
    hs = M.fromList as

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = maximum . map (happiness input) . seatingOrders $ persons input

--- Part 2

part2 :: Input -> Int
part2 input = maximum . map (happiness inputWithMyself) . seatingOrders $ persons inputWithMyself
  where
    inputWithMyself :: [Arrangement]
    inputWithMyself = input ++ concatMap (\p -> [(("myself", p), 0), ((p, "myself"), 0)]) (persons input)

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
