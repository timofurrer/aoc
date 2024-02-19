module Y15 (daySolvers) where

import qualified Y15.Day01 as Day01
import qualified Y15.Day02 as Day02
import qualified Y15.Day03 as Day03
import qualified Y15.Day04 as Day04
import qualified Y15.Day05 as Day05
import qualified Y15.Day06 as Day06
import qualified Y15.Day07 as Day07
import qualified Y15.Day08 as Day08
import qualified Y15.Day09 as Day09
import qualified Y15.Day10 as Day10
import qualified Y15.Day11 as Day11
import qualified Y15.Day12 as Day12
import qualified Y15.Day13 as Day13
import qualified Y15.Day14 as Day14
import qualified Y15.Day15 as Day15
import qualified Y15.Day16 as Day16
import qualified Y15.Day17 as Day17
import qualified Y15.Day18 as Day18

import Runner (DaySolver)

daySolvers :: [DaySolver]
daySolvers = [Day01.solve, Day02.solve, Day03.solve,
              Day04.solve, Day05.solve, Day06.solve,
              Day07.solve, Day08.solve, Day09.solve,
              Day10.solve, Day11.solve, Day12.solve,
              Day13.solve, Day14.solve, Day15.solve,
              Day16.solve, Day17.solve, Day18.solve
            ]
