from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools

DIGIT_SEGMENT_COUNTS = {1: 2, 4: 4, 7: 3, 8: 7}

data = [tuple(d.split() for d in x.split("|")) for x in puzzle_input_raw.splitlines()]
output_values = itertools.chain(*(d[1] for d in data))
digits = sum(len(d) in DIGIT_SEGMENT_COUNTS.values() for d in output_values)
print(digits)