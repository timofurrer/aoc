from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

lines = [tuple(map(int, re.fullmatch(r"(\d+),(\d+) -> (\d+),(\d+)", l).groups())) for l in puzzle_input_raw.splitlines()]

grid = set()
overlaps = set()


def process_lines(lines):
    for x1, y1, x2, y2 in lines:
        if x1 == x2:
            y1, y2 = sorted([y1, y2])
            yield {(x1, y) for y in range(y1, y2 + 1)}
        elif y1 == y2:
            x1, x2 = sorted([x1, x2])
            yield {(x, y1) for x in range(x1, x2 + 1)}
        else:
            if x1 > x2:
                x1, x2 = x2, x1
                y1, y2 = y2, y1
            if y2 > y1:
                yield {(x1 + n, y1 + n) for n in range(x2 + 1 - x1)}
            else:
                yield {(x1 + n, y1 - n) for n in range(x2 + 1 - x1)}


for line in process_lines(lines):
    overlaps |= grid & line
    grid |= line

print(len(overlaps))