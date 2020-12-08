import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# parse the boot code
boot_code = [x.split() for x in puzzle_input_raw.splitlines()]

# run the instructions
accumulator = 0
visited = set()
current_idx = 0
while current_idx not in visited:
    instruction = boot_code[current_idx]
    visited.add(current_idx)
    if instruction[0] == "acc":
        accumulator += int(instruction[1])
        current_idx += 1
    elif instruction[0] == "jmp":
        current_idx += int(instruction[1])
    else:
        current_idx += 1

print(f"Accumulator before recursion: {accumulator}")