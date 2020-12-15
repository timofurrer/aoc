import os
import sys
import re

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


memory = {}
current_mask = None
for instruction in puzzle_input_raw.splitlines():
    if instruction.startswith("mask = "):
        current_mask = instruction.split()[-1]
    else:
        if match := re.match(r"mem\[(\d+)\] = (\d+)$", instruction):
            address = match.group(1)
            value = f"{int(match.group(2)):036b}"

            v = "".join(m if m != "X" else v for m, v in zip(current_mask, value))
            memory[address] = int(v, base=2)

memory_sum = sum(memory.values())
print(memory_sum)