module Y15.Day16 (solve) where

-- Imports

import Lib.Debug

import Lib.Parsing

import qualified Data.Map as M

-- Input Parsing 

giftSue :: M.Map String Int
giftSue = M.fromList [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)]

type Input = [Sue]

type Sue = (Int, [(String, Int)])

parseInput :: String -> Input
parseInput = zip [1..] . parseLinesWith sue
  where
    sue = do
      _ <- symbol "Sue"
      _ <- number
      _ <- symbol ":"
      ts <- commaSeparated things
      pure ts

    things = do
      what <- identifier
      _ <- symbol ":"
      n <- number
      pure (what, n)


-- Common

isGiftSue :: ((String, Int) -> Bool) -> Sue -> Bool
isGiftSue f = all f . snd

findSue :: ((String, Int) -> Bool) -> Input -> Int
findSue f = fst . head . filter (isGiftSue f)

-- Solutions

--- Part 1

equal :: (String, Int) -> Bool
equal (s, i) = giftSue M.! s == i

part1 :: Input -> Int
part1 = findSue equal

--- Part 2

match :: (String, Int) -> Bool
match (s@"cats", i) = giftSue M.! s < i
match (s@"trees", i) = giftSue M.! s < i
match (s@"pomeranians", i) = giftSue M.! s > i
match (s@"goldfish", i) = giftSue M.! s > i
match (s, i) = giftSue M.! s == i

part2 :: Input -> Int
part2 = findSue match

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
