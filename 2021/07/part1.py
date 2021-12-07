from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import statistics

crab_positions = [int(x) for x in puzzle_input_raw.split(",")]

position = int(statistics.median(crab_positions))
fuel = sum(abs(x - position) for x in crab_positions)
print(fuel)