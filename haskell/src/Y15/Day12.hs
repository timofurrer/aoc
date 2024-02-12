module Y15.Day12 (solve) where

-- Imports

import Data.Ratio (denominator, numerator)
import Text.JSON

-- Input Parsing 

type Input = JSValue

parseInput :: String -> Input
parseInput raw = case decode raw of
                  Ok value -> value
                  Error errMsg -> error errMsg

-- Common

-- Solutions

--- Part 1

sumIntegers :: JSValue -> Int
sumIntegers (JSObject o)     = sum $ map (sumIntegers . snd) (fromJSObject o)
sumIntegers (JSArray a)      = sum $ map sumIntegers a
sumIntegers (JSRational _ r) = if denominator r == 1 then fromInteger $ numerator r else 0
sumIntegers _                = 0

part1 :: Input -> Int
part1 = sumIntegers

--- Part 2

sumIntegersNoRed :: JSValue -> Int
sumIntegersNoRed (JSObject o)     = let vs = map snd $ fromJSObject o
                                     in if any isRedValue vs then 0 else sum $ map (sumIntegersNoRed . snd) (fromJSObject o)
sumIntegersNoRed (JSArray a)      = sum $ map sumIntegersNoRed a
sumIntegersNoRed (JSRational _ r) = if denominator r == 1 then fromInteger $ numerator r else 0
sumIntegersNoRed _                = 0

isRedValue :: JSValue -> Bool
isRedValue (JSString str) = fromJSString str == "red"
isRedValue _ = False

part2 :: Input -> Int
part2 = sumIntegersNoRed

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
