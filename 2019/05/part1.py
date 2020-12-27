from pathlib import Path
import operator

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]
pointer = 0

OPERATORS = {1: operator.add, 2: operator.mul}

while program[pointer] != 99:
    instruction = str(program[pointer]).zfill(5)
    opcode = int(instruction[-2:])
    get_parameter = lambda i: program[program[pointer + i]] if list(reversed(instruction[:-2]))[i - 1] == "0" else program[pointer + i]
    if opcode in OPERATORS:
        op = OPERATORS[opcode]
        program[program[pointer + 3]] = op(get_parameter(1), get_parameter(2))
        pointer += 4
    elif opcode == 3:
        data = int(input("Please provide an integer: "))
        program[program[pointer + 1]] = data
        pointer += 2
    elif opcode == 4:
        data = get_parameter(1)
        print(data, end="")
        pointer += 2