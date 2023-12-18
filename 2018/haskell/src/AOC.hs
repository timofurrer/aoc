module AOC 
  (run)
where

import qualified AOC.Day01 as D01
import qualified AOC.Day02 as D02
import qualified AOC.Day03 as D03
import qualified AOC.Day07 as D07
import qualified AOC.Day08 as D08
import qualified AOC.Day09 as D09
import qualified AOC.Day10 as D10
import qualified AOC.Day11 as D11

placeholder :: String -> (String, String)
placeholder _ = ("placeholder", "placeholder")

daySolvers :: [String -> (String, String)]
daySolvers = [D01.solve, D02.solve, D03.solve,
              --D04.solve, D05.solve, D06.solve,
              placeholder, placeholder, placeholder,
              D07.solve, D08.solve, D09.solve,
              D10.solve, D11.solve
             ]

daySolver :: Int -> String -> (String, String)
daySolver day = daySolvers !! (day - 1)

run :: Int -> String -> (String, String)
run day = solve 
  where
    solve = daySolver day
