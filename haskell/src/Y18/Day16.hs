module Y18.Day16 (solve) where

-- Imports

import Lib.Debug

import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Bits ((.&.), (.|.))

import Text.Parsec

import Lib.Parsing 

-- Input Parsing 

type Input = ([Sample], [Opcode])

type Opcode = (Int, Int, Int, Int)

type Sample = (Registers, Opcode, Registers)

parseInput :: String -> Input
parseInput d = (parseWith samples d, parseLinesWith instr (last $ splitOn "\n\n\n\n" d))
  where
    samples = do
      many1 sample

    sample = do
      _ <- symbol "Before: ["
      before <- commaSeparated number
      _ <- symbol "]"
      opcode <- many1 number
      _ <- symbol "After:  ["
      after <- commaSeparated number
      _ <- symbol "]"
      pure (Map.fromList $ zip [0..3] before, toOpcode opcode, Map.fromList $ zip [0..3] after)

    instr = do
      opcode <- many1 number
      pure (toOpcode opcode)

    toOpcode [o, a, b, c] = (o, a, b, c)
    toOpcode [] = error "opcode has invalid format"

-- Common

type Registers = Map.Map Int Int

registers :: Registers
registers = Map.fromList $ map (,0) [0..3]

(!) :: Registers -> Int -> Int
rs ! k = rs Map.! k

setR :: Registers -> Int -> Int -> Registers
setR rs k v = Map.insert k v rs

type OpcodeF = Registers -> Int -> Int -> Int -> Registers

addr rs a b c = setR rs c (rs ! a + rs ! b)
addi rs a b c = setR rs c (rs ! a + b)

mulr rs a b c = setR rs c (rs ! a * rs ! b)
muli rs a b c = setR rs c (rs ! a * b)

banr rs a b c = setR rs c (rs ! a .&. rs ! b)
bani rs a b c = setR rs c (rs ! a .&. b)

borr rs a b c = setR rs c (rs ! a .|. rs ! b)
bori rs a b c = setR rs c (rs ! a .|. b)

setr rs a _ c = setR rs c (rs ! a)
seti rs a _ c = setR rs c a

gtir rs a b c = setR rs c (if a > rs ! b then 1 else 0)
gtri rs a b c = setR rs c (if rs ! a > b then 1 else 0)
gtrr rs a b c = setR rs c (if rs ! a > rs ! b then 1 else 0)

eqir rs a b c = setR rs c (if a == rs ! b then 1 else 0)
eqri rs a b c = setR rs c (if rs ! a == b then 1 else 0)
eqrr rs a b c = setR rs c (if rs ! a == rs ! b then 1 else 0)

opcodesF :: [Registers -> Int -> Int -> Int -> Registers]
opcodesF = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

opcodesI = [0..length opcodesF - 1]

-- Solutions

candidates :: [Int] -> Sample -> [Int]
candidates [] _ = []
candidates (op : ops) s@(before, (_, a, b, c), after) =
  if (opcodesF !! op) before a b c == after 
    then op : candidates ops s
    else candidates ops s

--- Part 1

part1 :: Input -> Int
part1 (samples, _) = length . filter (\cs -> length cs >= 3) $ map (candidates opcodesI) samples

--- Part 2

findOpcodes :: [Sample] -> Map.Map Int OpcodeF
findOpcodes = find Map.empty
  where
    find :: Map.Map Int Int -> [Sample] -> Map.Map Int OpcodeF
    find opcodeMap samples
      | Map.size opcodeMap == length opcodesF = Map.map ((!!) opcodesF) opcodeMap
      | otherwise = find opcodeMap' samples'
      where
        foundOpcodes = map snd . Map.toList
        notFoundOpcodes = opcodesI L.\\ foundOpcodes opcodeMap
        found = Map.fromList $ map (\(k, v) -> (k, head v)) $ filter (\(_, cs) -> length cs == 1) $ map (\s@(_, (o, _, _, _), _) -> (o, candidates notFoundOpcodes s)) samples
        opcodeMap' = opcodeMap `Map.union` found
        samples' = filter (\(_, (o, _, _, _), _) -> o `Map.notMember` found) samples

          
part2 :: Input -> Int
part2 (samples, instrs) = execute (findOpcodes samples) Map.! 0
  where
    execute opcodeMap = foldl (\acc (o, a, b, c) -> (opcodeMap Map.! o) acc a b c) registers instrs

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
