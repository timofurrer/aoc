from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re
from collections import defaultdict
import itertools

lights = defaultdict(int)
CMDS = {"toggle": lambda x: x + 2, "turn on": lambda x: x + 1, "turn off": lambda x: x - 1 if x > 0 else 0}

for cmd, sx, sy, dx, dy in (re.search(r"(.*?) (\d+),(\d+) through (\d+),(\d+)", x).groups() for x in puzzle_input_raw.splitlines()):
    for light_coord in itertools.product(range(int(sx), int(dx) + 1), range(int(sy), int(dy) + 1)):
        lights[light_coord] = CMDS[cmd](lights[light_coord])

lights_lit = sum(l for l in lights.values())
print(lights_lit)