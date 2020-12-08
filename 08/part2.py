import os
import sys
import copy

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

# parse the boot code
boot_code = [x.split() for x in puzzle_input_raw.splitlines()]


def run(boot_code):
    accumulator = 0
    visited = set()
    current_idx = 0

    while current_idx not in visited and current_idx < len(boot_code):
        instruction = boot_code[current_idx]
        visited.add(current_idx)
        if instruction[0] == "acc":
            accumulator += int(instruction[1])
            current_idx += 1
        elif instruction[0] == "jmp":
            current_idx += int(instruction[1])
        else:
            current_idx += 1

    return current_idx not in visited, accumulator


# brute force the shit
for idx, instruction in enumerate(boot_code):
    copied_boot_code = copy.deepcopy(boot_code)
    if instruction[0] == "nop":
        copied_boot_code[idx][0] = "jmp"
    elif instruction[0] == "jmp":
        copied_boot_code[idx][0] = "nop"

    is_terminated, accumulator = run(copied_boot_code)
    if is_terminated:
        print(f"Boot Code terminated with changed {instruction[0]} to {copied_boot_code[idx][0]} at {idx} - Accumulator is: {accumulator}")
        break