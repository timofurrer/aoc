from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

numbers = re.findall(r"[-+]?\d+", puzzle_input_raw)
print(sum(int(x) for x in numbers))