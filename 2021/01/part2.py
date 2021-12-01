from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import itertools

measurements = [int(m) for m in puzzle_input_raw.split()]

increases = sum(y > x for x, y in itertools.pairwise(
    map(sum, zip(measurements, measurements[1:], measurements[2:]))
))
print(increases)