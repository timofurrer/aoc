module Data.Grid where

-- Imports

import Lib.Point (Point(..), fourNeighborPoints)
import Lib.Grid

import qualified Data.Map.Strict as Map

data TestGrid = TestGrid
  { name :: !String
  , grid :: Grid Char
  , start :: !Point
  , paths :: Map.Map Point [[Point]]
  }
  deriving (Show)

testGridAdjacents :: TestGrid -> Point -> [Point]
testGridAdjacents (TestGrid _ g _ _) curr = 
  let filterInBounds = filter (isInBounds g) 
      filterObstacles = filter (\p -> g ! p /= '#')
      -- filterUnits = filter (\p -> g ! p /= 'U')
   in if g ! curr /= 'U'
        then filterObstacles . filterInBounds $ fourNeighborPoints curr
        else []

testGrid0 :: TestGrid
testGrid0 = TestGrid { name = "Test Grid 0",
                       grid = from2DList ["S...",
                                          "....",
                                          "....",
                                          "...."],
                       start = Point 0 0,
                       paths = Map.empty
                     }

testGrid1 :: TestGrid
testGrid1 = TestGrid { name = "Test Grid 1",
                       grid = from2DList ["S.U#U",
                                          ".#...",
                                          "U.#.U",
                                          "...#.",
                                          "UU..U"],
                       start = Point 0 0,
                       paths = Map.fromList [
                                (Point 2 0, [[Point 0 0, Point 1 0, Point 2 0]]),
                                (Point 0 2, [[Point 0 0, Point 0 1, Point 0 2]])
                               ] 
                     }

testGrid2 :: TestGrid
testGrid2 = TestGrid { name = "Test Grid 2",
                       grid = from2DList ["U.#U.U",
                                          ".#S.#.",
                                          "#..#..",
                                          "U#...U"],
                       start = Point 2 1,
                       paths = Map.fromList [
                                (Point 3 0, [[Point 2 1, Point 3 1, Point 3 0]]),
                                (Point 5 3, [[Point 2 1, Point 2 2, Point 2 3, Point 3 3, Point 4 3, Point 5 3]]),
                                (Point 5 0, [[Point 2 1, Point 2 2, Point 2 3, Point 3 3, Point 4 3, Point 4 2, Point 5 2, Point 5 1, Point 5 0]])
                               ] 
                     }

testGrid3 :: TestGrid
testGrid3 = TestGrid { name = "Test Grid 3",
                       grid = from2DList [".U#U.",
                                          "#.U##",
                                          "..S.#",
                                          "U....",
                                          "....U"],
                       start = Point 2 2,
                       paths = Map.fromList [
                                (Point 2 1, [[Point 2 2, Point 2 1]]),
                                (Point 1 0, [[Point 2 2, Point 1 2, Point 1 1, Point 1 0]]),
                                (Point 0 3, [[Point 2 2, Point 2 3, Point 1 3, Point 0 3],
                                             [Point 2 2, Point 1 2, Point 1 3, Point 0 3],
                                             [Point 2 2, Point 1 2, Point 0 2, Point 0 3]
                                            ]),
                                (Point 4 4, [[Point 2 2, Point 3 2, Point 3 3, Point 4 3, Point 4 4],
                                             [Point 2 2, Point 2 3, Point 3 3, Point 4 3, Point 4 4],
                                             [Point 2 2, Point 3 2, Point 3 3, Point 3 4, Point 4 4],
                                             [Point 2 2, Point 2 3, Point 3 3, Point 3 4, Point 4 4],
                                             [Point 2 2, Point 2 3, Point 2 4, Point 3 4, Point 4 4]
                                            ])
                               ] 
                     }

testGridBig :: TestGrid
testGridBig = TestGrid { name = "Test Grid Big",
                         grid = from2DList ["################################",
                                            "#######.......###.#.U###########",
                                            "#######...U#.####...#.##########",
                                            "#######....#####....U.##########",
                                            "######......#..#......##########",
                                            "#########U..U..#.......#########",
                                            "###########.........U..#########",
                                            "############.#.........#########",
                                            "###########..##.......##.##.####",
                                            "########U...##...U#............#",
                                            "#####..##...#........U.........#",
                                            "#####.U##........U.UU.........##",
                                            "#####..#......#####....#.......#",
                                            "#####.U......#######...........#",
                                            "#####...U...#########S.........#",
                                            "#####.....U.#########.........##",
                                            "####....U...#########.....#...##",
                                            "#.##........#########.....#.#.##",
                                            "#...........#########.....#.#..#",
                                            "#.U.#..##....#######..#####.#..#",
                                            "##U.......#...#####...#####.##.#",
                                            "#........#............#####..#.#",
                                            "##..#....#........#######..#...#",
                                            "#.....##.#........########.....#",
                                            "##....##...U.....##########....#",
                                            "###########.....###########....#",
                                            "###########.....##########.....#",
                                            "###########..#############.....#",
                                            "##########..#################..#",
                                            "##########.##################..#",
                                            "#########..##################.##",
                                            "################################"],
                         start = Point 21 14,
                         paths = Map.empty
                       }

-- testGrid4 :: TestGrid
-- testGrid4 = TestGrid { name = "Test Grid 4",
--                        grid = from2DList [".SU#.",
--                                           ".U.#.",
--                                           ".....",
--                                           "##.UU",
--                                           "U...#"],
--                        start = Point 0 1,
--                        paths = Map.fromList [
--                                 (Point 0 2, [[Point 0 1, Point 0 2]]),
--                                 (Point 1 1, [[Point 0 1, Point 1 1]]),
--                                 (Point 3 3, [[Point 0 1, Point 0 0, Point 1 0, Point 2 0, Point 2 1, Point 2 2, Point 2 3, Point 3 3],
--                                              [Point 0 1, Point 0 0, Point 1 0, Point 2 0, Point 2 1, Point 2 2, Point 3 2, Point 3 3]
--                                             ])
--                                ] 
--                      }
--
-- testGrid5 :: TestGrid
-- testGrid5 = TestGrid { name = "Test Grid 5",
--                        grid = from2DList ["...#.",
--                                           "S#..U",
--                                           ".U...",
--                                           "U#.U#",
--                                           "#..U."],
--                        start = Point 1 0,
--                        paths = Map.fromList [
--                                 (Point 2 1, [[Point 1 0, Point 2 0, Point 2 1]]),
--                                 (Point 3 0, [[Point 1 0, Point 2 0, Point 3 0]]),
--                                 (Point 1 4, [[Point 1 0, Point 0 0, Point 0 1, Point 0 2, Point 1 2, Point 1 3, Point 1 4]]),
--                                 (Point 3 3, [[Point 1 0, Point 0 0, Point 0 1, Point 0 2, Point 1 2, Point 1 3, Point 2 3, Point 3 3],
--                                              [Point 1 0, Point 0 0, Point 0 1, Point 0 2, Point 1 2, Point 2 2, Point 3 2, Point 3 3]
--                                             ])
--                                ] 
--                      }
--
-- testGrid6 :: TestGrid
-- testGrid6 = TestGrid { name = "Test Grid 3",
--                        grid = from2DList ["..#U.",
--                                           "#..##",
--                                           "..S.#",
--                                           "U....",
--                                           "....."],
--                        start = Point 2 2,
--                        paths = Map.fromList [
--                                 (Point 3 0, [[Point 2 2, Point 3 2, Point 3 1, Point 3 0],
--                                              [Point 2 2, Point 2 1, Point 2 0, Point 3 0]
--                                             ])
--                                ] 
--                      }
--
