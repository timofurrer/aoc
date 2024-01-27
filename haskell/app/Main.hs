module Main (main) where

import System.Environment

import qualified Data.Map.Strict as M

-- Import Runner
import Runner (DaySolver, run)

-- Import Years
import qualified Y18

years :: M.Map String [DaySolver]
years = M.fromList [("2018", Y18.daySolvers)]

-- Main
main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  case args of
    [year, day] -> do
        case years M.!? year of 
          Nothing -> print ("The year " ++ year ++ " does not exist!")
          Just daySolvers -> runYear daySolvers day contents
    [_] -> print "Please provide a day as second argument!"
    _ -> print "Please provide a year as first and a day as second argument!"

runYear :: [DaySolver] -> [Char] -> String -> IO ()
runYear daySolvers day contents = do
  let d = read day :: Int
  (part1, part2) <- run daySolvers d contents
  putStrLn "Part 1"
  putStrLn part1
  putStrLn "Part 2"
  putStrLn part2
