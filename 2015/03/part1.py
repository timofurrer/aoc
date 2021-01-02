from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import defaultdict

MOVES = {"^": (0, 1), ">": (1 ,0), "v": (0, -1), "<": (-1, 0)}
current_position = (0, 0)
houses = defaultdict(int)

for move in puzzle_input_raw:
    houses[current_position] += 1
    current_position = tuple(map(sum, zip(current_position, MOVES[move])))

at_least_one_present = sum(p >= 1 for p in houses.values())
print(at_least_one_present)