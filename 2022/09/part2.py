from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import math

grid = set()

ropes_pos = [(0, 0)] * 10

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
        ropes_pos[0] = MOVES[move](ropes_pos[0])
        for i in range(0, len(ropes_pos) - 1):
            prev, cur = ropes_pos[i], ropes_pos[i + 1]
            if distance(prev, cur) > 1.5:
                cur = tuple(map(sum, zip(cur, step(cur, prev))))
                ropes_pos[i + 1] = cur
                if i + 2 == len(ropes_pos):
                    grid.add(cur)

print(len(grid) + 1)