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


# find contiguous numbers to sum up to `number`
def add_up_to(number, numbers):
    sum_ = 0
    for i, n in enumerate(numbers):
        sum_ += n
        if sum_ == number:
            return i
        
    return None


for i in range(len(numbers)):
    end_idx = add_up_to(number, numbers[i:])
    if end_idx is not None:
        sorted_numbers = sorted(numbers[i:i + end_idx])
        print(f"{sorted_numbers[0] + sorted_numbers[-1]}")
        break