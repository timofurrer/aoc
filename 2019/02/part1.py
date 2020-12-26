from pathlib import Path
import operator

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]
# initial program changes
program[1] = 12
program[2] = 2
pointer = 0

OPERATORS = {1: operator.add, 2: operator.mul}

while program[pointer] != 99:
    op = OPERATORS[program[pointer]]
    program[program[pointer + 3]] = op(program[program[pointer + 1]], program[program[pointer + 2]])
    pointer += 4


print(program[0])