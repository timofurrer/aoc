import os
import sys
import re
import functools

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

grid = {}
for y, row in enumerate(puzzle_input_raw.splitlines()):
    for x, col in enumerate(row):
        grid[(x, y, 0)] = col


def get_neighbor_coords(x, y, z):
    return [
        (i, j, k)
        for i in range(x - 1, x + 2)
        for j in range(y - 1, y + 2)
        for k in range(z - 1, z + 2)
        if i != x or j != y or k != z
    ]

for _ in range(6):
    # expand grid dimensions
    expanded_grid = grid.copy()
    for coord in grid:
        neighbor_coords = get_neighbor_coords(*coord)
        for neighbor_coord in neighbor_coords:
            if neighbor_coord not in grid:
                expanded_grid[neighbor_coord] = "."

    grid = expanded_grid
    new_grid = {}
    for coord, state in grid.items():
        neighbor_fields = [grid.get(n) for n in get_neighbor_coords(*coord)]
        active_neighbors = neighbor_fields.count("#")

        # apply rules
        if state == "#":
            if active_neighbors in {2, 3}:
                new_grid[coord] = "#"
            else:
                new_grid[coord] = "."
        else:
            if active_neighbors == 3:
                new_grid[coord] = "#"
            else:
                new_grid[coord] = "."

    grid = new_grid

active_cubes = list(grid.values()).count("#")
print(active_cubes)