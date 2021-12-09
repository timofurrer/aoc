from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import deque
from functools import reduce

heightmap = {(r, c): int(col) for r, row in enumerate(puzzle_input_raw.splitlines()) for c, col in enumerate(row)}

ADJACENT = [(0, -1), (1, 0), (0, 1), (-1, 0)]

low_points = [
    p
    for p, n in heightmap.items()
    if all(n < heightmap.get(tuple(map(sum, zip(p, d))), 10) for d in ADJACENT)
]

basins = []
for low_point in low_points:
    to_explore = deque([low_point])
    basin = set()
    while to_explore:
        p = to_explore.popleft()
        basin.add(p)
        adjacent = [
            c
            for d
            in ADJACENT
            if (x := heightmap.get(c := tuple(map(sum, zip(p, d))))) and x < 9 and c not in basin
        ]
        to_explore.extend(adjacent)

    basins.append(basin)

largest_basins = sorted(basins, key=len, reverse=True)[:3]
total = reduce(lambda x, y: x * y, map(len, largest_basins))
print(total)