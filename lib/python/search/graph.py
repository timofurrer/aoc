"""
This module contains implementations for multiple Graph
variants. These implementations are meant to be used as
a basis for search optimization problems.
"""

from typing import Protocol, TypeVar, Dict, List, Tuple, Generator

#: Represents an addressable location in the Graph.
#  e.g. a location in a map (x, y) or a combination
#  from a permutation list.
Location = TypeVar("Location")

#: Represents a 2D-Location (x, y) in a grid
GridLocation = Tuple[int, int]


class Graph(Protocol[Location]):
    def children(self, loc: Location) -> List[Location]: ...

    def cost(self, from_loc: Location, to_loc: Location) -> float: ...


class SimpleGraph:
    def __init__(self):
        self.edges: Dict[Location, List[Location]] = {}
    
    def children(self, loc: Location) -> List[Location]:
        return self.edges[loc]

    def cost(self, from_loc: Location, to_loc: Location) -> float:
        assert to_loc in self.edges[from_loc], f"No edge from {from_loc} to {to_loc}"
        return 1


class WeightedUndirectedGraph:
    def __init__(self, edges, distances):
        self.edges = edges
        self.distances = distances

    def children(self, loc):
        return self.edges[loc]

    def cost(self, from_loc, to_loc):
        return self.distances[frozenset((from_loc, to_loc))]


def manhatten_distance(from_loc: GridLocation, to_loc: GridLocation):
    return abs(from_loc[0] - to_loc[0]) + abs(from_loc[1] - to_loc[1])


class GridGraph:
    # top-left: (0, 0)    NORTH    EAST   SOUTH    WEST
    DEFAULT_4_NEIGHBORS = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    def __init__(self):
        self.edges: Dict[GridLocation, List[GridLocation]] = {}

    def children(self, loc: GridLocation) -> List[GridLocation]:
        return [self.edges[l] for l in self.generate_4_neighbors(loc)]

    def cost(self, from_loc: Location, to_loc: Location) -> float: 
        assert manhatten_distance(from_loc, to_loc) == 1, f"Cost between non-neighbor locations is not supported"
        return 1 

    @classmethod
    def generate_4_neighbors(self, loc: Location, base_neighbors: List[GridLocation]=DEFAULT_4_NEIGHBORS) -> Generator[GridLocation, None, None]:
        yield from (tuple(map(sum, zip(loc, n))) for n in base_neighbors)