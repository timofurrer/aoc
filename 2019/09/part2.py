import sys
from pathlib import Path
import itertools
import timeit

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]
intcode = Intcode(program, inputs=[2])
keycode = intcode.run()
print(keycode[0])