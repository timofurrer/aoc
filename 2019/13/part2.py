import sys
from pathlib import Path
from queue import Queue

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]

score = None
paddle = None
ball = None
output_queue = Queue()

joystick = lambda: (ball > paddle) - (ball < paddle)
robot = Intcode(program, inputs=joystick, outputs=output_queue, initial_memory={0: 2})

while not robot.halted:
    x = robot.step_to_next_output()
    y = robot.step_to_next_output()
    tile_id_or_score = robot.step_to_next_output()

    if tile_id_or_score == 3:
        paddle = x
    elif tile_id_or_score == 4:
        ball = x
    if (x, y) == (-1, 0):
        score = tile_id_or_score

print(score)