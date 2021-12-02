from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import functools


COMMANDS = {
    "forward": lambda x, z, a, c: (x + c, z + a * c, a),
    "down": lambda x, z, a, c: (x, z, a + c),
    "up": lambda x, z, a, c: (x, z, a - c),
}

commands = [(c, int(d)) for c, d in (x.split() for x in puzzle_input_raw.split("\n"))]
x, z, _ = functools.reduce(
    lambda acc, x: COMMANDS[x[0]](*acc, x[1]), commands, (0, 0, 0)
)
result = x * z
print(result)