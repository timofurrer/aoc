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
coordinates = itertools.product(range(0, 50), range(0, 50))
grid = {coord: Intcode(program.copy(), inputs=list(coord)).run()[-1] for coord in coordinates}
print(sum(x for x in grid.values()))