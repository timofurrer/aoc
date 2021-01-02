from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with puzzle_input_path.open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

grid = {(x, y): s for y, row in enumerate(puzzle_input_raw.splitlines()) for x, s in enumerate(row)}

NEIGHBORS = [(0, 1), (1, 0), (0, -1), (-1, 0)]

seen = []
minutes = 0
while grid not in seen:
    seen.append(grid)
    new_grid = {}
    for coord, state in grid.items():
        adjacent_bugs = sum(grid.get(tuple(map(sum, zip(coord, n)))) == "#" for n in NEIGHBORS)

        if state == "#" and adjacent_bugs != 1:
            new_grid[coord] = "."
        elif state == "." and adjacent_bugs in {1, 2}:
            new_grid[coord] = "#"
        else:
            new_grid[coord] = state
        
    grid = new_grid
    minutes += 1

biodiversity = sum(pow(2, y * 5 + x) * (grid[(x, y)] == "#") for y in range(5) for x in range(5))
print(biodiversity)