module AOC 
  (run)
where

import qualified AOC.Day01 as Day01
import qualified AOC.Day02 as Day02
import qualified AOC.Day03 as Day03
import qualified AOC.Day07 as Day07
import qualified AOC.Day08 as Day08
import qualified AOC.Day09 as Day09
import qualified AOC.Day10 as Day10
import qualified AOC.Day11 as Day11
import qualified AOC.Day12 as Day12

placeholder :: String -> IO (String, String)
placeholder _ = return ("placeholder", "placeholder")

daySolvers :: [String -> IO (String, String)]
daySolvers = [Day01.solve, Day02.solve, Day03.solve,
              --Day04.solve, Day05.solve, Day06.solve,
              placeholder, placeholder, placeholder,
              Day07.solve, Day08.solve, Day09.solve,
              Day10.solve, Day11.solve, Day12.solve
             ]

daySolver :: Int -> String -> IO (String, String)
daySolver day = daySolvers !! (day - 1)

run :: Int -> String -> IO (String, String)
run day = solve 
  where
    solve = daySolver day
