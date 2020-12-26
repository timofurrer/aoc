import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

earliest = int(puzzle_input_raw.splitlines()[0])
bus_lines = [int(x) for x in puzzle_input_raw.splitlines()[1].split(",") if x != "x"]

earliest_bus = min(((x - (earliest % x), x) for x in bus_lines), key=lambda x: x[0])
print(earliest_bus[0] * earliest_bus[1])



# def closest_multiple(n, x):
#     n = n + x // 2 
#     n = n - (n % x)
#     return n

# multiples = ((closest_multiple(earliest, x), x) for x in bus_lines)
# earliest_bus = min(((k, v) for (k, v) in multiples if k >= earliest), key=lambda x: x[0])
# print((earliest_bus[0] - earliest) * earliest_bus[1])