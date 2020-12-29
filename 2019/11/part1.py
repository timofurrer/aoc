import sys
from pathlib import Path
from collections import deque, defaultdict
from concurrent.futures import ThreadPoolExecutor

sys.path.insert(0, str(Path(__file__).parent.parent))
import intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]

input_queue = deque()
output_queue = deque()
grid = defaultdict(int)
current_pos = (0, 0)
current_direction = (0, 1) 


def pop(q):
    while True:
        try:
            return q.popleft()
        except IndexError:
            continue

with ThreadPoolExecutor(max_workers=1) as executor:
    robot = executor.submit(intcode.run, program, inputs=input_queue, outputs=output_queue)

    while robot.running():
        input_queue.append(grid[current_pos])
        color = pop(output_queue)
        turn = pop(output_queue)
        grid[current_pos] = color
        if turn == 0:
            current_direction = current_direction[1] * -1, current_direction[0]
        else:
            current_direction = current_direction[1], current_direction[0] * -1
        current_pos = current_pos[0] + current_direction[0], current_pos[1] + current_direction[1]

    executor.shutdown()

print(len(grid))