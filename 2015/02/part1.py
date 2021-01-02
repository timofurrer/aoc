from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import math
import itertools

boxes = [tuple(int(x) for x in x.split("x")) for x in puzzle_input_raw.splitlines()]

needed_paper = sum(
    sum(2 * x * y for x, y in itertools.combinations(box, 2)) + math.prod(sorted(box)[:2])
    for box
    in boxes
)
print(needed_paper)
