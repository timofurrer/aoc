from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re
import operator
import functools
from collections import defaultdict

lines = [tuple(map(int, re.fullmatch(r"(\d+),(\d+) -> (\d+),(\d+)", l).groups())) for l in puzzle_input_raw.splitlines()]

grid = defaultdict(int)
inc = functools.partial(operator.add, 1)
dec = functools.partial(operator.add, -1)
step = lambda x1, x2: inc(x1) if x1 < x2 else dec(x1) if x1 > x2 else x1


for line in lines:
    x1, y1, x2, y2 = line
    p = (x1, y1)
    grid[p] += 1
    grid[(x2, y2)] += 1
    while (p := (step(p[0], x2), step(p[1], y2))) != (x2, y2):
        grid[p] += 1

least_two_overlaps = sum(1 for v in grid.values() if v >= 2)
print(least_two_overlaps)