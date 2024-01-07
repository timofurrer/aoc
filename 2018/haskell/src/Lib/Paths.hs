module Lib.Paths where

-- Imports

import Lib.Debug

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Map.Strict as SMap
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (|>))

import Lib.List (minimumsBy)


-- Types

type Distance = Int

type Parent a = a

type GraphWithDistances a = SMap.Map a (Distance, [Parent a])

-- Algorithms

--- BFS (Breadth-First Search)

bfs :: forall a. Ord a => a -> (a -> [a]) -> GraphWithDistances a
bfs start getAdjacents = bfs' (Seq.singleton (start, 0)) (SMap.singleton start (0, []))
  where
    bfs' :: Seq.Seq (a, Int) -> SMap.Map a (Int, [a]) -> SMap.Map a (Int, [a])
    bfs' queue visited
      | Seq.null queue = visited
      | otherwise      = 
        case Seq.viewl queue of 
          Seq.EmptyL -> visited -- this should never happen, because of the previous null guard.
          ((current, dist) :< restOfQueue) ->
            let adjacents = getAdjacents current
                (queue', visited') = foldl (update current dist) (restOfQueue, visited) adjacents
             in bfs' queue' visited'
      where
        update current dist (queue, visited) adjacent =
          let entry = SMap.findWithDefault (maxBound, []) adjacent visited
           in if fst entry > dist + 1
                then (queue |> (adjacent, dist + 1), SMap.insert adjacent (dist + 1, [current]) visited)
                else if fst entry == dist + 1
                        then (queue, SMap.insertWith (\new old -> (fst old, L.nub $ snd old ++ snd new)) adjacent (dist + 1, [current]) visited)
                        else (queue, visited)

-- Not sure if `SMap.filterWithKey` is the most ideal accessor here. It's probably better to just iterate targets 
filterReachableTargets :: Eq a => [a] -> GraphWithDistances a -> GraphWithDistances a
filterReachableTargets targets = SMap.filterWithKey (\k _ -> k `elem` targets)

minimumTargets :: Eq a => [a] -> GraphWithDistances a -> [a]
minimumTargets targets tree = 
  case L.uncons . minimumsBy (fst . snd) . M.toList $ filterReachableTargets targets tree of
    Nothing -> []
    Just (ts, _) -> map fst ts

reconstructPaths :: forall a. Ord a => GraphWithDistances a -> a -> [[a]]
reconstructPaths graph = map reverse . go'
  where
    go' node = 
      case SMap.lookup node graph of
        Nothing -> [] -- this shouldn't happen as the BfsGraph was preprocessed and should only contain reachable nodes.
        Just (_, []) -> [[node]] -- we've reached the source node
        Just (_, parents) -> concatMap (map (node :) . go') parents
