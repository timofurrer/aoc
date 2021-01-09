from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from collections import deque

RECT_PATTERN = re.compile(r"(\d+)x(\d+)")
ROT_ROW_PATTERN = re.compile(r"row y=(\d+) by (\d+)")
ROT_COL_PATTERN = re.compile(r"column x=(\d+) by (\d+)")

coords_for_square = lambda ex, ey: ((x, y) for y in range(ey) for x in range(ex))

display = [[False for _ in range(50)] for _ in range(6)]

for instruction in puzzle_input_raw.splitlines():
    if instruction.startswith("rect"):
        rx, ry = (int(x) for x in RECT_PATTERN.search(instruction).groups())
        for x, y in coords_for_square(rx, ry):
            display[y][x] = True
    elif instruction.startswith("rotate row"):
        y, shift = (int(x) for x in ROT_ROW_PATTERN.search(instruction).groups())
        shifted_row = deque(display[y])
        shifted_row.rotate(shift)
        display[y] = list(shifted_row)
    elif instruction.startswith("rotate column"):
        x, shift = (int(x) for x in ROT_COL_PATTERN.search(instruction).groups())
        display_T = list(map(list, zip(*display)))
        shifted_col = deque(display_T[x])
        shifted_col.rotate(shift)
        display_T[x] = list(shifted_col)
        display = list(map(list, zip(*display_T)))


for row in display:
    for c in row:
        print("O" if c else " ", end="")
    print()