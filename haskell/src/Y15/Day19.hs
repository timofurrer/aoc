module Y15.Day19 (solve) where

-- Imports

import Lib.Debug

import Lib.Parsing
import Lib.String (trim)

import Data.List (isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Lib.List (removeDups, pop)
import qualified Data.Set as S
import Data.Ord (comparing)

-- Input Parsing 

type Input = (String, [Transition])

type Transition = (String, String)

parseInput :: String -> Input
parseInput input = (start input, transitions input)
  where
    start = trim . last . splitOn "\n\n"
    transitions = parseLinesWith transition . head . splitOn "\n\n"
    transition = do
      from <- word
      _ <- symbol "=>"
      to <- word
      pure (from, to)


-- Common

replace :: String -> [Transition] -> [String]
replace = replace' ""
  where
    replace' :: String -> String -> [Transition] -> [String]
    replace' _ [] _ = []
    replace' prefix mol@(m : ms) ts = map (go prefix mol) (filter (\(from, _) -> from `isPrefixOf` mol) ts) ++ replace' (prefix ++ [m]) ms ts

    go prefix mol (from, to) = prefix ++ to ++ drop (length from) mol

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 (start, transitions) = length . removeDups $ replace start transitions

--- Part 2

find :: String -> String -> [Transition] -> Int
find start target ts = find' [(0, start)] S.empty
  where
    tl = length target
    find' :: [(Int, String)] -> S.Set String -> Int
    find' [] _             = error "didn't find any result"
    find' frontier visited =
      let ((n, cur), rest) = pop frontier
          rs = filter (`S.notMember` visited) . filter (\x -> length x <= tl) $ replace cur ts
       in if target `elem` rs `debug` ("n: " ++ show n)
            then n + 1
            else find' (sortBy (comparing fst) $ rest ++ map (n + 1,) rs) (visited `S.union` S.fromList rs)

part2 :: Input -> Int
part2 (target, transitions) = find "e" target transitions

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
