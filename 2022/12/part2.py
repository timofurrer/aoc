from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


from lib.search import (
    StartEndGraphProblem,
    dijkstra,
    reconstruct_path,
)

NEIGHBOR_DELTAS = [(1, 0), (0, -1), (-1, 0), (0, 1)]

grid = {(x, y): r for y, row in enumerate(puzzle_input_raw.splitlines()) for x, r in enumerate(row)}

POSSIBLE_STARTS = [c for c, v in grid.items() if v == "S" or v == "a"]
END = next(c for c, v in grid.items() if v == "E")


class Graph:
    def __init__(self, grid):
        self.grid = grid

    def patch(self, l):
        return ord("a" if l == "S" else "z" if l == "E" else l)

    def children(self, loc):
        c = self.patch(self.grid[loc])
        children = [n for d in NEIGHBOR_DELTAS if (n := tuple(map(sum, zip(loc, d)))) in self.grid and loc != n and self.patch(self.grid[n]) - c <= 1]
        return children

    def cost(self, from_, to):
        return 1


graph = Graph(grid)

starts = {}
for start in POSSIBLE_STARTS:
    problem = StartEndGraphProblem(graph, start, END)

    paths, _ = dijkstra(problem)
    if END in paths:
        shortest_path = reconstruct_path(paths, problem)
        starts[start] = len(shortest_path) - 1

print(min(starts.values()))