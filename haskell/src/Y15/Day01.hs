module Y15.Day01 (solve) where

-- Imports

-- Input Parsing 

type Input = [Char]

parseInput :: String -> Input
parseInput x = x

-- Common

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 = go 0
  where
    go n [] = n
    go n ('(' : xs) = go (n + 1) xs
    go n (')' : xs) = go (n - 1) xs
    go _ xs = error ("invalid input " ++ xs)

--- Part 2

part2 :: Input -> Int
part2 = go 0 0
  where
    go _ _ [] = error "invalid input; reached end floor before basement"
    go n f (x : xs)
      | n == -1   = f
      | x == '('  = go (n + 1) (f + 1) xs
      | x == ')'  = go (n - 1) (f + 1) xs
      | otherwise = error "invalid input"

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
