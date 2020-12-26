from pathlib import Path
import operator

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


OPERATORS = {1: operator.add, 2: operator.mul}
program = [int(x) for x in puzzle_input_raw.split(",")]

def run(program, noun, verb):
    # initial program changes
    program[1] = noun
    program[2] = verb
    pointer = 0

    while program[pointer] != 99:
        op = OPERATORS[program[pointer]]
        program[program[pointer + 3]] = op(program[program[pointer + 1]], program[program[pointer + 2]])
        pointer += 4

    return program[0]


for noun in range(100):
    for verb in range(100):
        if run(program.copy(), noun, verb) == 19690720:
            print(f"Found it: {100 * noun + verb}")
            exit()