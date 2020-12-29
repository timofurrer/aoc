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


def pop(q):
    while True:
        try:
            return q.popleft()
        except IndexError:
            continue

with ThreadPoolExecutor(max_workers=1) as executor:
    robot = executor.submit(intcode.run, program, inputs=input_queue, outputs=output_queue)

    while robot.running():
        x = pop(output_queue)
        y = pop(output_queue)
        tile_id = pop(output_queue)
        grid[(x, y)] = tile_id

    executor.shutdown()

print(sum(t == 2 for t in grid.values()))