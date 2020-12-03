import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

# parse puzzle input
grid = puzzle_input_raw.splitlines()
grid_width = len(grid[0])

# count trees
indices_gen = zip(range(3, len(grid) * 3, 3), range(1, len(grid), 1))

trees = sum(1 for col, row in indices_gen if grid[row][col % grid_width] == "#")
print(trees)
