from pathlib import Path
from itertools import cycle, accumulate

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

signal = [int(x) for x in puzzle_input_raw]
base_pattern = [0, 1, 0, -1]
offset = int(puzzle_input_raw[:7])

pattern = cycle(reversed(signal))
output_signal = [next(pattern) for _ in range(len(signal) * 10_000 - offset)]
for _ in range(100):
    output_signal = [x % 10 for x in accumulate(output_signal)]

print("".join(str(i) for i in output_signal[-1:-9:-1]))