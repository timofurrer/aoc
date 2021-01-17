from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from collections import Counter

coordinates = [tuple(int(x) for x in re.findall(r"\d+", l)) for l in puzzle_input_raw.splitlines()]

MAX_DISTANCE = 10_000

manhatten = lambda from_, to: abs(from_[0] - to[0]) + abs(from_[1] - to[1])

min_x, max_x = min(x for x, _ in coordinates), max(x for x, _ in coordinates)
min_y, max_y = min(y for _, y in coordinates), max(y for _, y in coordinates)

grid = {}
for y in range(min_y, max_y + 1):
    for x in range(min_x, max_x + 1):
        total_distance = sum(
            manhatten((x, y), (a, b)) for i, (a, b) in enumerate(coordinates)
        )
        grid[(x, y)] = total_distance < MAX_DISTANCE

print(sum(grid.values()))