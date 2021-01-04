from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

NEIGHBORS = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

grid = {(x, y): s == "#" for y, row in enumerate(puzzle_input_raw.splitlines()) for x, s in enumerate(row)}

for _ in range(100):
    new_grid = grid.copy()
    for coord, s in grid.items():
        on_neighbors = sum(grid.get(tuple(map(sum, zip(coord, n))), False) for n in NEIGHBORS)

        if s and on_neighbors not in {2, 3}:
            new_grid[coord] = False
        elif not s and on_neighbors == 3:
            new_grid[coord]  = True
    grid = new_grid

print(sum(x for x in grid.values()))