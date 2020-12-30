import sys
from pathlib import Path
from collections import defaultdict
from queue import Queue

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]

input_queue = Queue()
output_queue = Queue()
grid = defaultdict(int)

robot = Intcode(program, inputs=input_queue, outputs=output_queue)

while not robot.halted:
    x = robot.step_to_next_output()
    y = robot.step_to_next_output()
    tile_id = robot.step_to_next_output()
    grid[(x, y)] = tile_id

print(sum(t == 2 for t in grid.values()))