import os
import sys
from functools import reduce
from collections import defaultdict

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# Using Axial-Coordinates as described here and recommended for storage:
# https://www.redblobgames.com/grids/hexagons/
# +q points to ne, where +r points to s in straight lines
DIRECTIONS = {
    # (q, r)
    "ne": (1, -1),
    "e": (1, 0), 
    "se": (0, 1), 
    "sw": (-1, 1), 
    "w": (-1, 0), 
    "nw": (0, -1),
}

def consume_directions(directions):
    while directions:
        next_dir, coords = next((d, c) for d, c in DIRECTIONS.items() if directions.startswith(d))
        directions = directions[len(next_dir):]
        yield coords
        

floor = defaultdict(lambda: True)
for directions in puzzle_input_raw.splitlines():
    target_tile_coords = reduce(lambda x, y: (x[0] + y[0], x[1] + y[1]), consume_directions(directions), (0, 0))
    floor[target_tile_coords] = not floor[target_tile_coords]

black_tiles = list(floor.values()).count(False)
print(black_tiles)