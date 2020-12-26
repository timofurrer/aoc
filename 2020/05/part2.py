import os
import sys
import math

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


seats = sorted([
    int(s.translate(str.maketrans({"F": "0", "L": "0", "B": "1", "R": "1"})), base=2)
    for s
    in puzzle_input_raw.splitlines()
])
missing_seat = next(x for x in range(seats[0], seats[-1]) if x not in seats)
print(missing_seat)