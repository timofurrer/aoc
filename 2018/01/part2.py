from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools

frequencies = set()
frequency = 0
for change in itertools.cycle(int(x) for x in puzzle_input_raw.splitlines()):
    frequency += change
    if frequency in frequencies:
        break
    frequencies.add(frequency)

print(frequency)