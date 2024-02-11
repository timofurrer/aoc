module Y15.Day06 (solve) where

-- Imports

import Lib.Grid (Grid, updateWith, allPointsWithValue, fromNMWithDefault)
import Lib.Point (pointsInRectangle)
import Lib.Parsing

import Text.Parsec

-- Input Parsing 

type Input = [Instruction]

data Instruction = Instruction
  { cmd :: !Cmd
  , from :: !Range
  , to :: !Range
  }
  deriving (Show)

data Cmd
  = On
  | Off
  | Toggle
  deriving (Show)

type Range = (Int, Int)

parseInput :: String -> Input
parseInput = parseLinesWith instruction
  where
    instruction = do
      c <- symbol "turn on" <|> symbol "turn off" <|> symbol "toggle"
      f1 <- number
      _ <- symbol ","
      f2 <- number
      _ <- symbol "through"
      t1 <- number
      _ <- symbol ","
      t2 <- number
      pure $ Instruction {cmd=parseCmd c, from=(f1, f2), to=(t1, t2)}

parseCmd :: String -> Cmd
parseCmd s
  | s == "turn on"  = On
  | s == "turn off" = Off
  | s == "toggle"   = Toggle
  | otherwise       = error "invalid cmd in input"

-- Common

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = length . filter (\(_, v) -> v == 1) . allPointsWithValue . go . reverse
  where
    emptyGrid :: Grid Int
    emptyGrid = fromNMWithDefault 1000 1000 0

    go [] = emptyGrid
    go ((Instruction c f t) : xs) = updateWith (dispatch c) (pointsInRectangle f t) $ go xs

    dispatch On = turnOn
    dispatch Off = turnOff
    dispatch Toggle = toggle

    turnOn _ _  = 1 
    turnOff _ _ = 0
    toggle _ 0  = 1
    toggle _ 1  = 0
    toggle _ _  = error "invalid input"

--- Part 2
          
part2 :: Input -> Int
part2 = sum . map snd . allPointsWithValue . go . reverse
  where
    emptyGrid :: Grid Int
    emptyGrid = fromNMWithDefault 1000 1000 0

    go [] = emptyGrid
    go ((Instruction c f t) : xs) = updateWith (dispatch c) (pointsInRectangle f t) $ go xs

    dispatch On     = turnOn
    dispatch Off    = turnOff
    dispatch Toggle = toggle

    turnOn _ v  = v + 1
    turnOff _ 0 = 0
    turnOff _ v = v - 1
    toggle _ v  = v + 2

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
