module Lib.Graph where

-- Graph is a type class that represents a Graph data structure
-- and is able to return some adjacent vertices / nodes for a given vertex / node.
class Graph a where
  -- adjacents returns for a given node 'a' a list of adjacent nodes
  adjacents :: a -> [a]

