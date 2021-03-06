import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# parse puzzle input
group_answers = puzzle_input_raw.split(os.linesep + os.linesep)

count = sum(
    len(set.intersection(*(set(x) for x in g.splitlines())))
    for g
    in group_answers
)

print(count)