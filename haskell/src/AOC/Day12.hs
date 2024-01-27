module AOC.Day12 (solve) where

-- Imports

import Lib.Debug

import Lib.Parsing

import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
import Lib.List (consecutiveDiff)

-- Input Parsing 

type Input = (GenState, Rules)

type GenState = M.Map Int Char

type Rules = M.Map [Char] Char

parseInput :: String -> Input
parseInput rawInput = (M.fromList $ zip [0..] $ parseWith initialState rawInitialState, M.fromList $ parseListWith rule rawRules)
  where
    rawInitialState = head $ take 1 $ lines rawInput
    rawRules = drop 2 $ lines rawInput

    initialState = do
      _ <- symbol "initial state: "
      many1 (char '#' <|> char '.')

    rule = do
      s <- many1 (char '#' <|> char '.')
      _ <- symbol " => "
      t <- char '#' <|> char '.'
      pure (s, t)
    

-- Part 1

evolvePlant :: GenState -> Rules -> Int -> (Int, Char)
evolvePlant state rules x = (x, M.findWithDefault '.' pots rules)
  where
    pots = map (\x' -> M.findWithDefault '.' x' state) [x - 2..x + 2]

evolve :: Int -> GenState -> Rules -> [GenState]
evolve = go 0
  where
    go gen maxGen state rules
      | gen == maxGen + 1 = []
      | otherwise = let newState = M.fromList $ trim $ map (evolvePlant state rules) [fst (M.findMin state)-3..fst (M.findMax state)+3]
                     in state : go (gen + 1) maxGen newState rules
        where 
          trim = L.dropWhileEnd f . L.dropWhile f
            where 
              f (_, c) = c == '.'

genSum :: GenState -> Int
genSum = M.foldrWithKey (\k v acc -> if v == '.' then acc else acc + k) 0 

part1 :: Input -> Int
part1 (state, rules) = genSum . last $ evolve 20 state rules

-- Part 2
          
part2 :: Input -> Int
part2 (state, rules) = last sums + (50000000000 - 300) * cdiff
  where
    sums = map genSum $ evolve 300 state rules
    cdiff = last $ consecutiveDiff sums 

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
