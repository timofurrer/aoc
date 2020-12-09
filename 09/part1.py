import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

preamble = 25
numbers = [int(x) for x in puzzle_input_raw.splitlines()]

number = next(
    n 
    for i, n
    in enumerate(numbers[preamble:], start=preamble)
    if all(n - e not in numbers[i - preamble:i] for e in numbers[i - preamble:i])
)
print(number)

# for idx, number in enumerate(numbers[preamble:], start=preamble):
#     last_numbers = numbers[idx - preamble:idx]
#     does_not_match = all(number - e not in last_numbers for e in last_numbers)
#     if does_not_match:
#         break