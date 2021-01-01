import sys
from pathlib import Path
from collections import deque
import itertools

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]

is_pulled = lambda coord: Intcode(program.copy(), inputs=list(coord)).run()[-1]

pos = (0, 0)
while not is_pulled((pos[0] + 99, pos[1])):
    pos = pos[0], pos[1] + 1
    while not is_pulled((pos[0], pos[1] + 99)):
        pos = pos[0] + 1, pos[1]

print(pos[0] * 10_000 + pos[1])