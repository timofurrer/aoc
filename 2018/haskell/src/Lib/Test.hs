module Lib.Test where

import Lib.Debug

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>), ViewL(..))
import Data.List (nub)

type Point = (Int, Int)  -- Coordinates on the grid (x, y)
type Grid = [String]     -- Grid representation, e.g., ["..#.", ".E..", "...U"]

isValid :: Grid -> Point -> Bool
isValid grid (x, y) =
    let inBounds = y >= 0 && y < length grid && x >= 0 && x < length (grid !! y)
        notObstacle = (grid !! y !! x) /= '#'
    in inBounds && notObstacle

getNeighbors :: Point -> [Point]
getNeighbors (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

bfs :: Grid -> Point -> Map.Map Point (Int, [Point])
bfs grid start = bfs' (Seq.singleton (start, 0)) (Map.singleton start (0, [])) where
    bfs' queue visited
        | Seq.null queue = visited
        | otherwise = 
            case Seq.viewl queue of
                EmptyL -> visited
                ((point, dist) :< restQueue) -> 
                    let neighbors = filter (isValid grid) $ getNeighbors point
                        (newQueue, newVisited) = foldl (processNeighbor point dist) (restQueue, visited) neighbors
                    in bfs' newQueue newVisited

    processNeighbor point dist (queue, visited) neighbor =
        let entry = Map.findWithDefault (maxBound, []) neighbor visited
        in if fst entry > dist
           then (queue |> (neighbor, dist + 1), Map.insert neighbor (dist + 1, [point]) visited)
           else if fst entry == dist
                then (queue, Map.insertWith (\new old -> (fst old, nub $ snd old ++ snd new)) neighbor (dist + 1, [point]) visited)
                else (queue, visited)

reconstructPaths :: Map.Map Point (Int, [Point]) -> Point -> [[Point]]
reconstructPaths visited target = nub $ reconstruct visited target where
    reconstruct visited target =
        case Map.lookup target visited of
            Just (_, []) -> [[target]]
            Just (_, preds) -> concatMap (\p -> map (target :) (reconstruct visited p)) preds
            Nothing -> []

findEnemies :: Grid -> [Point]
findEnemies grid = [(x, y) | y <- [0..length grid - 1], x <- [0..length (grid !! y) - 1], (grid !! y !! x) == 'E']

findPathsToEnemies :: Grid -> Point -> [[Point]]
findPathsToEnemies grid start =
    let visited = bfs grid start
        enemies = findEnemies grid
        paths = concatMap (reconstructPaths visited) enemies
    in filter (not . null) paths


main :: IO ()
main = do
    let grid = [ "..........",
                 "..E#......",
                 ".......#..",
                 ".....E....",
                 "..U.......",
                 "..........",
                 "......U...",
                 "..........",
                 ".......#..",
                 ".........."]

    let start = (2, 4)  -- Assuming 'U' is at position (2, 4)

    putStrLn "Grid:"
    mapM_ putStrLn grid

    let paths = findPathsToEnemies grid start
    putStrLn "\nShortest Paths to Enemies:"
    mapM_ print paths
