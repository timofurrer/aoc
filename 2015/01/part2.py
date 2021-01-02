from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools

instructions = {"(": lambda x: x + 1, ")": lambda x: x - 1}
floor = 0
def walk(x):
    global floor
    floor = instructions[x](floor)
    return floor >= 0
basement_position = len(list(itertools.takewhile(walk, puzzle_input_raw))) + 1
print(basement_position)