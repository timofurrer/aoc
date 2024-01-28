module Y15.Day02 (solve) where

-- Imports

import qualified Data.List as L
import qualified Data.List.Split as LS

-- Input Parsing

type Input = [GiftDimensions]

type GiftDimensions = (Int, Int, Int)

parseInput :: String -> Input
parseInput = map (giftDimensions . LS.splitOn "x") . lines
  where
    giftDimensions [l, w, h] = (read l :: Int, read w :: Int, read h :: Int)
    giftDimensions _ = error "invalid input"

-- Common

dim :: Num a => (a, a, a) -> a
dim (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l

slack :: (Ord a, Num a) => (a, a, a) -> a
slack (l, w, h) = minimum [l * w, w * h, h * l]

ribbon :: (Num a, Ord a) => (a, a, a) -> a
ribbon (l, w, h) = bow + 2 * head r' + 2 * (r' !! 1)
  where
    r' = L.sort [l, w, h]
    bow = l * w * h

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = sum . map (\gd -> dim gd + slack gd)

--- Part 2

part2 :: Input -> Int
part2 = sum . map ribbon

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
