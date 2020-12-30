from pathlib import Path
from itertools import cycle

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

signal = [int(x) for x in puzzle_input_raw]
base_pattern = [0, 1, 0, -1]

def phase(input_signal):
    output_signal = []
    for i, s in enumerate(signal, start=1):
        pattern = cycle([y for x in base_pattern for y in [x] * i])
        next(pattern)  # drop very first
        output_signal.append(
            int(str(sum(x * y for x, y in zip(input_signal, pattern)))[-1])
        )
    return output_signal


for _ in range(100):
    signal = phase(signal)

print("".join(str(x) for x in signal[:8]))