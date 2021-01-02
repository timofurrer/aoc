from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import functools

instructions = {"(": lambda x: x + 1, ")": lambda x: x - 1}
floor = functools.reduce(lambda x, y: instructions[y](x), puzzle_input_raw, 0)
print(floor)