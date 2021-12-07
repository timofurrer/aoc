from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import math
import statistics

crab_positions = [int(x) for x in puzzle_input_raw.split(",")]

# NOTE(TF): don't know if flooring or ceiling is correct.
#           demo input needs ceiling, but my input needs flooring.
#           therefore test both within +- 0.5
positions = [math.floor(statistics.mean(crab_positions) + d) for d in (0.5, -0.5)]
calc_fuel = lambda p: sum(abs(x - p) * (abs(x - p) + 1) / 2 for x in crab_positions)
fuel = int(min(calc_fuel(p) for p in positions))
print(fuel)