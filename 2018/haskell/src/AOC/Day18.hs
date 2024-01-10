module AOC.Day18 (solve) where

-- Imports

import Lib.Debug

import Data.List (elemIndex)
import qualified Data.Map as Map

import Lib.List (counter)
import Lib.Grid
import Lib.Point
import Data.Maybe (fromMaybe)

-- Input Parsing 

type Input = Grid Char

parseInput :: String -> Input
parseInput = parseFromString

-- Common

adjacents :: Grid Char -> Point -> [Point]
adjacents = eightAdjacents

counterMap :: Ord a => [a] -> Map.Map a Int
counterMap = Map.fromList . counter

evolve :: Grid Char -> Point -> Char -> Char
evolve g p '.'
  | length (filter (\p' -> g ! p' == '|') $ adjacents g p) >= 3 = '|'
  | otherwise                                                   = '.'
evolve g p '|'
  | length (filter (\p' -> g ! p' == '#') $ adjacents g p) >= 3 = '#'
  | otherwise                                                   = '|'
evolve g p '#'
  | Map.findWithDefault 0 '#' cs >= 1 && Map.findWithDefault 0 '|' cs >= 1 = '#'
  | otherwise                                                              = '.'
  where
    cs = counterMap $ map (g !) $ adjacents g p 
evolve _ _ _ = error "invalid acre"

evolveArea :: Grid Char -> Grid Char
evolveArea g = updateAllWith (evolve g) g

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 input = 
  let
    area = foldl (\acc _ -> evolveArea acc) input [0 :: Int ..9]
  in nwood area * nlumb area `debug` ("Acre\n\n" ++ display area)
  where
    nwood g = length . filter (\(_, v) -> v == '|') $ allPointsWithValue g
    nlumb g = length . filter (\(_, v) -> v == '#') $ allPointsWithValue g

--- Part 2
          
part2 :: Input -> Int
part2 input = 
  let
    acres = takeWhile (\(_, _, _, m) -> not m) $ scanl (\(_, g, s, _) i -> let g' = evolveArea g in if g' `elem` s then (i, g', s, True) else (i, g', s ++ [g'], False)) (0, input, [input], False) [1 :: Int ..1000000000]
    (x2', g', s, _) = last acres
    g = evolveArea g'
    -- cycle detection
    x2 = x2' + 1
    x1 = fromMaybe 0 $ elemIndex g s
    period = x2 - x1
    (_, area, _, _) = acres !! (x1 + ((1000000000 - x1) `mod` period))
  in nwood area * nlumb area
  where
    nwood g = length . filter (\(_, v) -> v == '|') $ allPointsWithValue g
    nlumb g = length . filter (\(_, v) -> v == '#') $ allPointsWithValue g

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
