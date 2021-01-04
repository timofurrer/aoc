from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

grid = {(x, y): s == "#" for y, row in enumerate(puzzle_input_raw.splitlines()) for x, s in enumerate(row)}

NEIGHBORS = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
CORNERS = [(0, 0), (max(x for x, _ in grid), 0), (0, max(y for _, y in grid)), (max(x for x, _ in grid), max(y for _, y in grid))]
for corner in CORNERS:
    grid[corner] = True

for _ in range(100):
    new_grid = grid.copy()
    for coord, s in ((c, s) for c, s in grid.items() if c not in CORNERS):
        on_neighbors = sum(grid.get(tuple(map(sum, zip(coord, n))), False) for n in NEIGHBORS)

        if s and on_neighbors not in {2, 3}:
            new_grid[coord] = False
        elif not s and on_neighbors == 3:
            new_grid[coord]  = True
    grid = new_grid

print(sum(x for x in grid.values()))