from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

NEIGHBORS = (0, -1), (1, 0), (0, 1), (-1, 0)

grid = [list(int(x) for x in l) for l in puzzle_input_raw.splitlines()]
GRID_MAX_ROWS = len(grid)
GRID_MAX_COLS = len(grid[0])

import itertools

def walk(s, r):
    for d in itertools.repeat(r):
        y, x  = (s := tuple(map(sum, zip(s, d))))
        if y < 0 or x < 0 or y >= GRID_MAX_ROWS or x >= GRID_MAX_COLS:
            break
        yield s

count = len(grid) * 2 + len(grid[0]) * 2 -4
for y, row in enumerate(grid[1:-1], start=1):
    for x, cell in enumerate(row[1:-1], start=1):
        count += next((1 for n in NEIGHBORS if not any(grid[cur[0]][cur[1]] >= cell for cur in walk((y, x), n))), 0)

print(count)