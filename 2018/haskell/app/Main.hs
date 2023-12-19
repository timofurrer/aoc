module Main (main) where

import System.Environment

import qualified AOC

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  case args of
    [day] -> do
        let d = read day :: Int
        (part1, part2) <- AOC.run d contents
        putStrLn "Part 1"
        putStrLn part1
        putStrLn "Part 2"
        putStrLn part2
    _ -> print "Please provide a day as first argument!"
