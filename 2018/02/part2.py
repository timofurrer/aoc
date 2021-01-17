from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools

box_ids = puzzle_input_raw.splitlines()

fabric_boxes = next(
    (b, o) 
    for b, o in itertools.combinations(box_ids, 2) 
    if sum(x == y for x, y in zip(b, o)) == len(b) - 1
)

print("".join(a for a, b in zip(*fabric_boxes) if a == b))