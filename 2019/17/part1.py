import sys
from pathlib import Path
from collections import defaultdict, deque
from itertools import cycle

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]

NEIGHBORS = [(0, 1), (1, 0), (0, -1), (-1, 0)]

intcode = Intcode(program)
ascii_grid_raw = [chr(x) for x in intcode.run()]
cols = ascii_grid_raw.index("\n")
ascii_grid = [a for a in ascii_grid_raw if a != "\n"]
rows = len(ascii_grid) // cols
grid = {}

for y in range(rows):
    for x in range(cols):
        grid[(x, y)] = ascii_grid[y * cols + x]


intersections = []
for xy, a in (g for g in grid.items() if g[1] == "#"):
    neighbors = (tuple(map(sum, zip(xy, d))) for d in NEIGHBORS)
    if all(grid.get(n) == "#" for n in neighbors):
        intersections.append(xy)

print(sum(x * y for x, y in intersections))