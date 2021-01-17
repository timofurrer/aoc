from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from collections import Counter

coordinates = [tuple(int(x) for x in re.findall(r"\d+", l)) for l in puzzle_input_raw.splitlines()]

manhatten = lambda from_, to: abs(from_[0] - to[0]) + abs(from_[1] - to[1])

min_x, max_x = min(x for x, _ in coordinates), max(x for x, _ in coordinates)
min_y, max_y = min(y for _, y in coordinates), max(y for _, y in coordinates)

grid = {}
for y in range(min_y, max_y + 1):
    for x in range(min_x, max_x + 1):
        closest = sorted(
            {i: manhatten((x, y), (a, b)) for i, (a, b) in enumerate(coordinates)}.items(), 
            key=lambda x: x[1]
        )
        grid[(x, y)] = closest[0][0] if closest[0][1] < closest[1][1] else "."


edge_groups = {
    v 
    for (x, y), v in grid.items() 
    if x == min_x or x == max_x or y == min_y or y == max_y 
}
# remove edges
no_edges = {
    k: v 
    for k, v in grid.items() 
    if v not in edge_groups and v != "."
}

print(Counter(no_edges.values()).most_common(1))