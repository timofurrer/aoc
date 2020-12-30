import sys
from pathlib import Path
from collections import defaultdict, deque
from copy import deepcopy
from enum import IntEnum

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]

DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]
COMMANDS = [1, 4, 2, 3]

class State(IntEnum):
    WALL = 0
    FREE = 1
    OXYGEN_SYSTEM = 2

start_position = (0, 0)
positions_to_travel = deque([start_position])
droids = {start_position: Intcode(program)}
grid = {}

while positions_to_travel:
    travel_position = positions_to_travel.pop()
    for direction, cmd in zip(DIRECTIONS, COMMANDS):
        next_position = tuple(map(sum, zip(travel_position, direction)))
        if next_position in droids:
            continue  # already traveled to this position

        droid = deepcopy(droids[travel_position])
        droid.inputs.append(cmd)
        state = droid.step_to_next_output()
        droids[next_position] = droid
        grid[next_position] = State(state)
        if state != 0:
            positions_to_travel.appendleft(next_position)


positions = deque([(0, start_position)])
visited = set()

while positions:
    movements, position = positions.pop()
    visited.add(position)
    for direction in DIRECTIONS:
        next_position = tuple(map(sum, zip(position, direction)))
        if next_position in visited:
            continue  # already traveled to this position

        state = grid.get(next_position, State.WALL)
        if state is State.OXYGEN_SYSTEM:
            print(movements + 1)
            break

        if state != State.WALL:
            positions.appendleft((movements + 1, next_position))


# def draw(grid):
#     row_min, row_max = min(grid, key=lambda g: g[1])[1], max(grid, key=lambda g: g[1])[1]
#     col_min, col_max = min(grid, key=lambda g: g[0])[0], max(grid, key=lambda g: g[0])[0]

#     for y in reversed(range(row_min - 1, row_max + 1)):
#         for x in range(col_min -1, col_max + 1):
#             state = grid.get((x, y), 0)
#             if state is State.FREE:
#                 print(" . ", end="")
#             elif state is State.OXYGEN_SYSTEM:
#                 print(" O ", end="")
#             else:
#                 print(" # ", end="")
#         print()