module Y15.Day18 (solve) where

-- Imports

import Lib.Debug

import Lib.Grid
import Lib.Point

-- Input Parsing 

type Input = Grid Char

parseInput :: String -> Input
parseInput = parseFromString

-- Common

animate :: (Grid Char -> Point -> Char -> Char) -> Grid Char -> Grid Char
animate f g = iterate (\ acc -> updateAllWith (f acc) acc) g !! 100

update :: Grid Char -> Point -> Char -> Char
update g p c
  | c == '#'  = if 2 == on || 3 == on then '#' else '.'
  | c == '.'  = if 3 == on then '#' else '.'
  | otherwise = error "invalid light state"
  where
    on = length . filter ('#'==) $ map (g !) $ eightAdjacents g p

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = length . filter ('#'==) . allValues . animate update

--- Part 2

updateNoCorner :: Grid Char -> Point -> Char -> Char
updateNoCorner g p c
  | isCorner g p = c
  | c == '#'  = if 2 == on || 3 == on then '#' else '.'
  | c == '.'  = if 3 == on then '#' else '.'
  | otherwise = error "invalid light state"
  where
    on = length . filter ('#'==) $ map (g !) $ eightAdjacents g p

part2 :: Input -> Int
part2 input = length . filter ('#'==) . allValues $ animate updateNoCorner inputWithOnCorners
  where
    inputWithOnCorners = updateWith (\_ _ -> '#') (corners input) input

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
