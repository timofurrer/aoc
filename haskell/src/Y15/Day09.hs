module Y15.Day09 (solve) where

-- Imports

import Lib.List (removeDups)
import Lib.Parsing

import qualified Data.Map as M
import Data.List (permutations)

-- Input Parsing 

type Input = [AtoB]

type AtoB = ((String, String), Int)

parseInput :: String -> Input
parseInput = bothWays . parseLinesWith routes
  where
    routes = do
      a <- word
      _ <- symbol "to"
      b <- word
      _ <- symbol "="
      d <- number
      pure ((a, b), d)

    bothWays :: [AtoB] -> [AtoB]
    bothWays [] = []
    bothWays (x@((a, b), d) : xs) = x : ((b, a), d) : bothWays xs

-- Common

locations :: [AtoB] -> [String]
locations [] = []
locations (((a, b), _) : xs) = removeDups $ [a, b] ++ locations xs

routeLength :: M.Map (String, String) Int -> [String] -> Int
routeLength distances route = snd . foldl (\(r, d) x -> (x, d + distances M.! (r, x))) (head route, 0) $ tail route

travelingSalesman :: [AtoB] -> [Int]
travelingSalesman routeDistances = map (routeLength distances) allRoutes
  where
    distances = M.fromList routeDistances
    allRoutes = permutations $ locations routeDistances

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = minimum . travelingSalesman

--- Part 2
          
part2 :: Input -> Int
part2 = maximum . travelingSalesman

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
