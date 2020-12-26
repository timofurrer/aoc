import os
import sys
import re
import itertools

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

            
def expand_floating_address(floating_address):
    expanded = ((x,) if x != "X" else ("0", "1") for x in floating_address)
    return (int("".join(x), base=2) for x in itertools.product(*expanded))


memory = {}
current_mask = None
for instruction in puzzle_input_raw.splitlines():
    if instruction.startswith("mask = "):
        current_mask = instruction.split()[-1]
    else:
        if match := re.match(r"mem\[(\d+)\] = (\d+)$", instruction):
            address = f"{int(match.group(1)):036b}"
            value = int(match.group(2))

            floating_address = "".join(m if m != "0" else a for m, a in zip(current_mask, address))
            for address in expand_floating_address(floating_address):
                memory[address] = value

memory_sum = sum(memory.values())
print(memory_sum)