from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from lib.search import (
    StartEndGraphProblem,
    dijkstra,
    reconstruct_path,
)

NEIGHBOR_DELTAS = [(1, 0), (0, -1), (-1, 0), (0, 1)]

grid = {(x, y): int(r) for y, row in enumerate(puzzle_input_raw.splitlines()) for x, r in enumerate(row)}

START = (0, 0)
END = (max(c[0] for c in grid.keys()), max(c[1] for c in grid.keys()))


class Graph:
    def __init__(self, grid):
        self.grid = grid

    def children(self, loc):
        return [n for d in NEIGHBOR_DELTAS if (n := tuple(map(sum, zip(loc, d)))) in self.grid]

    def cost(self, from_, to):
        return self.grid[to]



graph = Graph(grid)
problem = StartEndGraphProblem(graph, START, END)

paths, _ = dijkstra(problem)
shortest_path = reconstruct_path(paths, problem)
print(sum(grid[x] for x in shortest_path) - grid[START])