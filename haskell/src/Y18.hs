module Y18 (daySolvers) where

import qualified Y18.Day01 as Day01
import qualified Y18.Day02 as Day02
import qualified Y18.Day03 as Day03
import qualified Y18.Day07 as Day07
import qualified Y18.Day08 as Day08
import qualified Y18.Day09 as Day09
import qualified Y18.Day10 as Day10
import qualified Y18.Day11 as Day11
import qualified Y18.Day12 as Day12
import qualified Y18.Day13 as Day13
import qualified Y18.Day14 as Day14
import qualified Y18.Day15 as Day15
import qualified Y18.Day16 as Day16

import qualified Y18.Day18 as Day18
import qualified Y18.Day19 as Day19
import qualified Y18.Day20 as Day20

import Runner (DaySolver, placeholder)

daySolvers :: [DaySolver]
daySolvers = [Day01.solve, Day02.solve, Day03.solve,
              placeholder, placeholder, placeholder,
              Day07.solve, Day08.solve, Day09.solve,
              Day10.solve, Day11.solve, Day12.solve,
              Day13.solve, Day14.solve, Day15.solve,
              Day16.solve, placeholder, Day18.solve,
              Day19.solve, Day20.solve
            ]
