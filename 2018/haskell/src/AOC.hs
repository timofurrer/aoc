module AOC 
  (run)
where

import qualified AOC.Day01 as D01
import qualified AOC.Day02 as D02
import qualified AOC.Day03 as D03

daySolvers :: [String -> (String, String)]
daySolvers = [D01.solve, D02.solve, D03.solve]

daySolver :: Int -> String -> (String, String)
daySolver day = daySolvers !! (day - 1)

run :: Int -> String -> (String, String)
run day = solve 
  where
    solve = daySolver day
