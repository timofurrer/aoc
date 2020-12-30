import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]


def run(program, noun, verb):
    intcode = Intcode(program, initial_memory={1: noun, 2: verb})
    intcode.run()
    return intcode[0]


for noun in range(100):
    for verb in range(100):
        if run(program.copy(), noun, verb) == 19690720:
            print(f"Found it: {100 * noun + verb}")
            exit()