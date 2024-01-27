module AOC.Day11 (solve, powerLevel, nthDigit) where

-- Imports

import Lib.Debug

import Data.List
import Data.Function
import qualified Data.Map as M

-- Main

solve :: String -> IO (String, String)
solve input = 
  return (show $ part1 $ parse input, 
   show $ part2 $ parse input
  )

-- Input Parsing 

type Input = Int

parse :: String -> Input
parse = read

-- Part 1

part1 :: Input -> (Int, Int)
part1 serialNumber = fst $ maximumBy (compare `on` snd) $ fuelCells 3 (grid 3) $ powerLevels serialNumber

grid n = [ (x, y) | x <- [1..300], y <- [1..300], x + (n -1) <= 300 && y + (n -1) <= 300 ]

powerLevels serialNumber = M.fromList $ map (\p -> (p, powerLevel serialNumber p)) [ (x, y) | x <- [1..300], y <- [1..300] ]

fuelCells :: Int -> [(Int, Int)] -> M.Map (Int, Int) Int -> [((Int, Int), Int)]
fuelCells _ [] _ = []
fuelCells n ((x, y) : xs) power = ((x, y), fuelCell n (x, y) power) : fuelCells n xs power

fuelCell :: Int -> (Int, Int) -> M.Map (Int, Int) Int -> Int
fuelCell n (x, y) power = sum $ map (\p -> M.findWithDefault 0 p power) points
  where
    points = [ (x', y') | x' <- [x..x+n-1], y' <- [y..y+n-1] ]
        

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serialNumber (x, y) = subtract 5 $ nthDigit 2 $ ((x + 10) * y + serialNumber) * (x + 10)

nthDigit :: Int -> Int -> Int
nthDigit n number = number `div` 10 ^ n `mod` 10
  

-- Part 2

part2 :: Input -> (Int, Int, Int)
part2 serialNumber = let (x, y, n) = fst $ maximumBy (compare `on` snd) $ go grids $ partialSums M.empty $ M.toList $ powerLevels serialNumber
                      in (x-n+1, y-n+1, n)
  where
    grids = [ (x, y, n) | x <- [1..300], y <- [1..300], n <- [1..300], x >= n && y >= n ]

    go [] _ = []
    go ((x, y, n) : xs) ps =  ((x, y, n), c - n' - w + nw) : go xs ps
      where
        c = M.findWithDefault 0 (x, y) ps
        n' = M.findWithDefault 0 (x, y - n) ps
        w = M.findWithDefault 0 (x - n, y) ps
        nw = M.findWithDefault 0 (x - n, y - n) ps

    partialSums :: M.Map (Int, Int) Int -> [((Int, Int), Int)] -> M.Map (Int, Int) Int
    partialSums ps [] = ps
    partialSums ps ((k, v) : xs) = let ps' = M.insert k (go ps k v) ps
                                    in M.union ps' $ partialSums ps' xs
      where
        go ps (x, y) v = v + n + w - nw
          where
            n = M.findWithDefault 0 (x, y - 1) ps
            w = M.findWithDefault 0 (x - 1, y) ps
            nw = M.findWithDefault 0 (x - 1, y - 1) ps
          
part2bruteFoce :: Input -> ((Int, Int), Int)
part2bruteFoce serialNumber = go 1
  where
    power = powerLevels serialNumber

    go :: Int -> ((Int, Int), Int)
    go n 
      | n == 10 + 1 = ((0,0), n) 
      | otherwise    = let cMax = maximumBy (compare `on` snd) $ fuelCells n (grid n) $ power `debug` ("Finished " ++ show n)
                           nMax = go (n + 1) 
                        in if snd cMax > snd nMax
                            then cMax 
                            else nMax
