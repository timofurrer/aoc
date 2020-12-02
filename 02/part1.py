import re
import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# convert to list of inputs
puzzle_input = [re.search(r"(\d+)-(\d+) ([a-z]): (.*)", x).groups() for x in puzzle_input_raw.splitlines()]

correct = sum(int(lowest) <= password.count(char) <= int(highest) for lowest, highest, char, password in puzzle_input)
# for lowest, highest, char, password in puzzle_input:
    # occurances = password.count(char)
    # if int(lowest) <= occurances <= int(highest):
        # correct += 1


print(correct)
