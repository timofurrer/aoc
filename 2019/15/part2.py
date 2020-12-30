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


oxygen_system_position = next(p for p, s in grid.items() if s == State.OXYGEN_SYSTEM)
positions = deque([oxygen_system_position])
visited = set()
minute = 0

while positions:
    next_minute = deque()
    while positions:
        position = positions.pop()
        visited.add(position)
        for direction in DIRECTIONS:
            next_position = tuple(map(sum, zip(position, direction)))
            if next_position in visited:
                continue  # already traveled to this position

            state = grid.get(next_position, State.WALL)
            if state is State.FREE:
                next_minute.appendleft(next_position)
    positions = next_minute
    minute += 1

print(minute - 1)