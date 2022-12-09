from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import math

grid = set()

head_pos = (0, 0)
tail_pos = (0, 0)

MOVES = {
    "U": lambda p: (p[0], p[1] - 1),
    "D": lambda p: (p[0], p[1] + 1),
    "L": lambda p: (p[0] - 1, p[1]),
    "R": lambda p: (p[0] + 1, p[1]),
}

distance = lambda a, b: math.sqrt(math.pow(a[0] - b[0], 2) + math.pow(a[1] - b[1], 2))
cap = lambda d: 1 if d > 0 else -1 if d < 0 else 0
step = lambda a, b: (cap(b[0] - a[0]), cap(b[1] - a[1]))

for move, amount in (l.split() for l in puzzle_input_raw.splitlines()):
    amount = int(amount)
    for i in range(amount):
        head_pos = MOVES[move](head_pos)
        if distance(head_pos, tail_pos) > 1.5:
            tail_pos = tuple(map(sum, zip(tail_pos, step(tail_pos, head_pos))))
            grid.add(tail_pos)

print(len(grid) + 1)