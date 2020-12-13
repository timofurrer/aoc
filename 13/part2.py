import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "test.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

bus_lines = [(i, int(x)) for i, x in enumerate(puzzle_input_raw.splitlines()[1].split(",")) if x != "x"]

previous_bus_period = 1
current_timestamp = 0
for timestamp_offset, bus in bus_lines:
    # find matching period point for new bus
    while (current_timestamp + timestamp_offset) % bus:
        current_timestamp += previous_bus_period
    # combine the `bus` with the previous buses to match up period
    previous_bus_period *= bus

print(current_timestamp)