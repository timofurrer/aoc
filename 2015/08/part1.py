from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

total = 0
for code_string in puzzle_input_raw.splitlines():
    code_length = len(code_string)
    memory_repr = code_string[1:-1].replace("\\\\", "\\").replace('\\"', '"')
    memory_repr = re.sub("\\\\x[a-fA-f0-9]{2}", "?", memory_repr)
    memory_length = len(memory_repr)
    total += code_length - memory_length

print(total)