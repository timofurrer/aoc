import sys
import math

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

# parse puzzle input
grid = puzzle_input_raw.splitlines()
grid_width = len(grid[0])

# slopes
slopes = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2),
]


def create_slope_gen(right, down):
    return zip(range(right, len(grid * right), right), range(down, len(grid), down))


def count_trees(slope_gen):
    return sum(1 for col, row in slope_gen if grid[row][col % grid_width] == "#")


count_trees_gen = (count_trees(create_slope_gen(right, down)) for right, down in slopes)
multiplied_tree_count_for_all_slopes = math.prod(count_trees_gen)

print(multiplied_tree_count_for_all_slopes)
