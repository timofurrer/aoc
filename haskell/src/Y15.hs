module Y15 (daySolvers) where

import qualified Y15.Day01 as Day01
import qualified Y15.Day02 as Day02
import qualified Y15.Day03 as Day03
import qualified Y15.Day04 as Day04
import qualified Y15.Day05 as Day05
import qualified Y15.Day06 as Day06

import Runner (DaySolver)

daySolvers :: [DaySolver]
daySolvers = [Day01.solve, Day02.solve, Day03.solve,
              Day04.solve, Day05.solve, Day06.solve
            ]
