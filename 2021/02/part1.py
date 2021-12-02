from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import functools


COMMANDS = {
    "forward": lambda x, z, c: (x + c, z),
    "down": lambda x, z, c: (x, z + c),
    "up": lambda x, z, c: (x, z - c),
}

commands = [(c, int(d)) for c, d in (x.split() for x in puzzle_input_raw.split("\n"))]
x, z = functools.reduce(
    lambda acc, x: COMMANDS[x[0]](*acc, x[1]), commands, (0, 0)
)
result = x * z
print(result)