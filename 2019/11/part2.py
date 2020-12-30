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

# Setup Robot Intcode program
input_queue = Queue()
output_queue = Queue()
robot = Intcode(program, inputs=input_queue, outputs=output_queue)

grid = defaultdict(int)
current_pos = (0, 0)
current_direction = (0, 1) 
grid[current_pos] = 1

while not robot.halted:
    input_queue.put(grid[current_pos])
    color = robot.step_to_next_output()
    turn = robot.step_to_next_output()
    grid[current_pos] = color
    if turn == 0:
        current_direction = current_direction[1] * -1, current_direction[0]
    else:
        current_direction = current_direction[1], current_direction[0] * -1
    current_pos = current_pos[0] + current_direction[0], current_pos[1] + current_direction[1]


row_min, row_max = min(grid, key=lambda g: g[1])[1], max(grid, key=lambda g: g[1])[1]
col_min, col_max = min(grid, key=lambda g: g[0])[0], max(grid, key=lambda g: g[0])[0]

for y in reversed(range(row_min - 1, row_max + 1)):
    for x in range(col_min -1, col_max + 1):
        if grid.get((x, y), 0) == 1:
            print(" O ", end="")
        else:
            print("   ", end="")
    print()