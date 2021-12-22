from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import re
import itertools


reboot_steps = []
for line in puzzle_input_raw.splitlines():
   mode, *coords = re.match(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)", line).groups()
   reboot_steps.append((mode, (int(coords[0]), int(coords[1])), (int(coords[2]), int(coords[3])), (int(coords[4]), int(coords[5]))))

grid = set()

for mode, *cuboid_boundaries in reboot_steps:
    if any(l < -50 or u > 50 for l, u in cuboid_boundaries):
        continue
    cuboid_coords = set(itertools.product(*map(lambda x: range(x[0], x[1]+1), cuboid_boundaries)))
    if mode == "on":
        grid |= cuboid_coords
    else:
        grid -= cuboid_coords


print(len(grid))