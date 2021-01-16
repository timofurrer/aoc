from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import defaultdict
import re
import itertools

from lib.search import StartEndGraphProblem, dijkstra, reconstruct_path, GridGraph

nodes_raw = [x.split() for x in puzzle_input_raw.splitlines()[2:]]
nodes = defaultdict()
for node, *specs in nodes_raw:
    x, y = tuple(int(x) for x in re.search(r"x(\d+)-y(\d+)", node).groups())
    nodes[(x, y)] = tuple(int(s[:-1]) for s in specs)

grid = {k: v[-1] == 0 for k, v in nodes.items()}

def draw(grid):
    row_min, row_max = min(grid, key=lambda g: g[1])[1], max(grid, key=lambda g: g[1])[1]
    col_min, col_max = min(grid, key=lambda g: g[0])[0], max(grid, key=lambda g: g[0])[0]
    print(row_max, col_max)

    for y in reversed(range(row_min - 1, row_max + 1)):
        for x in range(col_min -1, col_max + 1):
            state = grid.get((x, y), 0)
            if state:
                print("_", end="")
            else:
                print(".", end="")
        print()

draw(grid)