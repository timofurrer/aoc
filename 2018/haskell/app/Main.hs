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
       in print $ AOC.run d contents
    _ -> print "Please provide a day as first argument!"
