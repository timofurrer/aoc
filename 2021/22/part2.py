from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import re
from collections import Counter, defaultdict


reboot_steps = []
for line in puzzle_input_raw.splitlines():
   mode, *coords = re.match(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)", line).groups()
   reboot_steps.append((mode, (int(coords[0]), int(coords[1])), (int(coords[2]), int(coords[3])), (int(coords[4]), int(coords[5]))))


cubes = Counter()
for mode, *cuboid_boundaries in reboot_steps:
    cubes_update = defaultdict(int)
    for cuboid_coord, cuboid_mode in cubes.items():
        intersection_x = (max(cuboid_boundaries[0][0], cuboid_coord[0][0]), min(cuboid_boundaries[0][1], cuboid_coord[0][1]))
        intersection_y = (max(cuboid_boundaries[1][0], cuboid_coord[1][0]), min(cuboid_boundaries[1][1], cuboid_coord[1][1]))
        intersection_z = (max(cuboid_boundaries[2][0], cuboid_coord[2][0]), min(cuboid_boundaries[2][1], cuboid_coord[2][1]))
        if intersection_x[0] <= intersection_x[1] and intersection_y[0] <= intersection_y[1] and intersection_z[0] <= intersection_z[1]:
            cubes_update[(intersection_x, intersection_y, intersection_z)] -= cuboid_mode
    if mode == "on":
        cubes_update[tuple(cuboid_boundaries)] += 1
    cubes.update(cubes_update)


total_on = sum(
    (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1) * m
    for ((x0, x1), (y0, y1), (z0, z1)), m
    in cubes.items()
)
print(total_on)