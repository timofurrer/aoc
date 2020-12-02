import re
import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# convert to list of inputs
puzzle_input = [re.search(r"(\d+)-(\d+) ([a-z]): (.*)", x).groups() for x in puzzle_input_raw.splitlines()]

correct = 0
for first, second, char, password in puzzle_input:
    if (password[int(first) - 1] == char) != (password[int(second) - 1] == char):
        correct += 1


print(correct)
