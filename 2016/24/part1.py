from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools

from lib.search import (
    StartEndGraphProblem, 
    GridGraph, 
    dijkstra, 
    reconstruct_path, 
    WeightedUndirectedGraph, 
    travelling_salesman_problem,
)

grid = {(x, y): c for y, r in enumerate(puzzle_input_raw.splitlines()) for x, c in enumerate(r)}

# get all number targets
targets = {p: c for p, c in grid.items() if c.isdigit()}
# get all combinations of paths
hops = [(s, e) for s, e in itertools.combinations(targets, 2) if s != e]

class Map(GridGraph):
    def __init__(self, grid):
        self.grid = grid

    def children(self, loc):
        return [n for n in self.generate_4_neighbors(loc) if self.grid.get(n, "#") != "#"]

graph = Map(grid)

shortest_paths = {}
for start, end in hops:
    problem = StartEndGraphProblem(graph, start, end)
    paths, _ = dijkstra(problem)
    shortest_paths[frozenset((grid[start], grid[end]))] = len(reconstruct_path(paths, problem)) - 1

edges = {x: [y for y in targets.values() if y != x] for x in targets.values()}
tsp_graph = WeightedUndirectedGraph(edges, shortest_paths)
print(travelling_salesman_problem(tsp_graph, "0"))