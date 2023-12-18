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
          parts = AOC.run d contents
       in do 
        putStrLn "Part 1"
        putStrLn $ fst parts
        putStrLn "Part 2"
        putStrLn $ snd parts
    _ -> print "Please provide a day as first argument!"
