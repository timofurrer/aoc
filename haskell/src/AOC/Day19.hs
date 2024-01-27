module AOC.Day19 (solve) where

-- Imports

import Lib.Debug

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Bits ((.&.), (.|.))

import Text.Parsec

import Lib.Parsing 

-- Input Parsing 

type Input = (IP, [Instr])

type IP = Int

type Instr = (String, Int, Int, Int)

parseInput :: String -> Input
parseInput d = (parseWith ip d, parseListWith instr (tail $ lines d))
  where
    ip = do
      _ <- symbol "#ip"
      number

    instr = do
      opcode <- identifier
      params <- many1 number
      pure (toInstr opcode params)

    toInstr opcode [a, b, c] = (opcode, a, b, c)
    toInstr _ [] = error "instruction has invalid format"

-- Common

type Registers = Map.Map Int Int

registers :: Registers
registers = Map.fromList $ map (,0) [0..5]

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

opcodes :: Map.Map String OpcodeF
opcodes = Map.fromList [("addr", addr), ("addi", addi), ("mulr", mulr), ("muli", muli), ("banr", banr), ("bani", bani), ("borr", borr), ("bori", bori), ("setr", setr), ("seti", seti), ("gtir", gtir), ("gtri", gtri), ("gtrr", gtrr), ("eqir", eqir), ("eqri", eqri), ("eqrr", eqrr)]

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 (ipr, instrs) = execute 0 registers
  where
    execute ip rs
      | ip >= length instrs = rs ! 0
      | otherwise           = 
        let (o, a, b, c) = instrs !! ip
            rs' = setR rs ipr ip
            rs'' = (opcodes Map.! o) rs' a b c
            ip' = (rs'' ! ipr) + 1
         in execute ip' rs''
      

--- Part 2

part2 :: Input -> Int
part2 (ipr, instrs) = execute 0 (Map.fromList [(0, 1), (1, 0), (2, 0), (3, 0), (4, 10551305), (5, 0)])
  where
    execute ip rs
      | ip >= length instrs = rs ! 0
      | otherwise           = 
        let (o, a, b, c) = instrs !! ip
            rs' = setR rs ipr ip
            rs'' = (opcodes Map.! o) rs' a b c
            ip' = (rs'' ! ipr) + 1
         in execute ip' rs'' `debug` ("Opcode: " ++ show o ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " regs " ++ show rs)

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
