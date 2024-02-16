module Y15.Day14 (solve) where

-- Imports

import Lib.Parsing
import Data.List (maximumBy)
import Data.Function (on)
import Lib.List (maximumsBy)

-- Input Parsing 

type Input = [((Int, Int), Int)]

parseInput :: String -> Input
parseInput = parseLinesWith reindeer
  where
    reindeer = do
      _ <- word
      _ <- symbol "can fly"
      v <- number
      _ <- symbol "km/s for"
      t <- number
      _ <- symbol "seconds, but then must rest for"
      r <- number
      pure ((v, t), r)

-- Common

-- Solutions

distance :: ((Int, Int), Int) -> Int
distance ((v, t), r) = v * t * x
  where
    x = let (d, m) = 2503 `divMod` (t + r) in if m >= t then d + 1 else d

--- Part 1

part1 :: Input -> Int
part1 = maximum . map distance

--- Part 2

type Stat = (Int, Int)

type ReindeerStat = (((Int, Int), Int), Stat)

dist :: ReindeerStat -> Int
dist = fst . snd

points :: ReindeerStat -> Int
points = snd . snd

part2 :: Input -> Int
part2 = go 0 . map (\r -> (r, (0, 0)) :: ReindeerStat)
  where
    advance :: Int -> ReindeerStat -> ReindeerStat
    advance n (((v, t), r), (x, p)) = (((v, t), r), (x', p))
      where
        x' = if n `mod` (t + r) < t then x + v else x

    second :: Int -> [ReindeerStat] -> [ReindeerStat]
    second n = map (advance n)

    go :: Int -> [ReindeerStat] -> Int
    go n rs
      | n == 2503 = (snd . snd) $ maximumBy (compare `on` points) rs
      | otherwise = let rs' = second n rs
                        rp = maximumsBy dist rs'
                        rs'' = concat $ tail rp ++ [map (\(r, (x, p)) -> (r, (x, p + 1))) (head rp)]
                     in go (n + 1) rs''

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
