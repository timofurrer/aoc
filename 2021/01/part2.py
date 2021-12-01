from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import functools

measurements = [int(m) for m in puzzle_input_raw.split()]

measurement_windows = list(
    map(sum, zip(measurements, measurements[1:], measurements[2:]))
)

increases = functools.reduce(
    lambda acc, x: (x, acc[1] + 1) if acc[0] < x else (x, acc[1]),
    measurement_windows,
    (measurement_windows[0], 0)
)
print(increases[1])