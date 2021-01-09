from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import functools

KEYPAD = {
    (0, 0): "1", (1, 0): "2", (2, 0): "3",
    (0, 1): "4", (1, 1): "5", (2, 1): "6",
    (0, 2): "7", (1, 2): "8", (2, 2): "9",
}
MOVES = {
    "U": (0, -1),
    "R": (1, 0),
    "D": (0, 1),
    "L": (-1, 0)
}

moves = puzzle_input_raw.splitlines()

clip = lambda x, y: (max(0, min(2, x)), max(0, min(2, y)))

code = []
current_position = (1, 1)
for m in moves:
    current_position = functools.reduce(lambda p, n: clip(*tuple(map(sum, zip(p, MOVES[n])))), m, current_position)
    code.append(KEYPAD[current_position])

print("".join(code))