import os
import sys
import math

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


seats = sorted([
    int(s.replace("F", "0").replace("L", "0").replace("B", "1").replace("R", "1"), base=2)
    for s
    in puzzle_input_raw.splitlines()
])
missing_seat = next(x for x in range(seats[0], seats[-1]) if x not in seats)
print(missing_seat)