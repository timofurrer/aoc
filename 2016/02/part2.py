from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import functools

KEYPAD = {
                              (2, 0): "1", 
                 (1, 1): "2", (2, 1): "3", (3, 1): "4",
    (0, 2): "5", (1, 2): "6", (2, 2): "7", (3, 2): "8", (4, 2): "9",
                 (1, 3): "A", (2, 3): "B", (3, 3): "C",
                              (2, 4): "D",

}
MOVES = {
    "U": (0, -1),
    "R": (1, 0),
    "D": (0, 1),
    "L": (-1, 0)
}

moves = puzzle_input_raw.splitlines()

code = []
current_position = (0, 2)
for m in moves:
    def move(p, n):
        x = tuple(map(sum, zip(p, MOVES[n])))
        return x if x in KEYPAD else p

    current_position = functools.reduce(move, m, current_position)
    code.append(KEYPAD[current_position])

print("".join(code))