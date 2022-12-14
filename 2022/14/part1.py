from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re
import itertools

PATH_COMP = re.compile(r"(\d+),(\d+)")

grid = {}

def draw_line(a, b):
    ax, ay = a
    bx, by = b
    if ax == bx:  # vertical
        for dy in range(min(ay, by), max(ay, by) + 1):
            grid[(ax, dy)] = "#"
    if ay == by:  # horizontal
        for dx in range(min(ax, bx), max(ax, bx) + 1):
            grid[(dx, ay)] = "#"

for p in ([eval(p) for p in l.split(" -> ")] for l in puzzle_input_raw.splitlines()):
    for a, b in zip(p, p[1:]):
        draw_line(a, b)

bottom_y = max(k[1] for k in grid.keys())

MOVES = [(0, 1), (-1, 1), (1, 1)]
SAND_START = (500, 0)

for i in itertools.count():
    s = SAND_START
    while s[1] < bottom_y:
        if ns := next((ns for nsd in MOVES if (ns := tuple(map(sum, zip(s, nsd)))) not in grid), None):
            s = ns
        else:
            grid[s] = "o"
            break
    else:
        break

print(i)