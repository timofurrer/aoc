import sys
from pathlib import Path
from collections import deque
import re

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]

NEIGHBORS = [(0, -1), (1, 0), (0, 1), (-1, 0)]

def direction(current_d, next_d):
    d = NEIGHBORS.index(current_d) - NEIGHBORS.index(next_d)
    if d == 1 or d < 0 and d != -1:
        return "L"
    elif d == -1 or d > 0 and d != 1:
        return "R"
    else:
        assert False, "fucked up"
        

intcode = Intcode(program.copy())
ascii_grid_raw = [chr(x) for x in intcode.run()]
cols = ascii_grid_raw.index("\n")
ascii_grid = [a for a in ascii_grid_raw if a != "\n"]
rows = len(ascii_grid) // cols
grid = {}

for y in range(rows):
    for x in range(cols):
        grid[(x, y)] = ascii_grid[y * cols + x]


intersections = []
for xy, a in (g for g in grid.items() if g[1] == "#"):
    neighbors = (tuple(map(sum, zip(xy, d))) for d in NEIGHBORS)
    if all(grid.get(n) == "#" for n in neighbors):
        intersections.append(xy)


for y in range(rows):
    for x in range(cols):
        print(grid.get((x, y), " "), end="")
    print()

movements = []
start_position = next(p for p, a in grid.items() if a == "^")
current_position = start_position
previous_position = start_position
current_direction = NEIGHBORS[0]

while any(tuple(map(sum, zip(current_position, d))) != previous_position for d in NEIGHBORS if grid.get(tuple(map(sum, zip(current_position, d)))) == "#"):
    # check if turn is required
    if grid.get(tuple(map(sum, zip(current_position, current_direction)))) != "#":
        neighbors = ((tuple(map(sum, zip(current_position, d))), d) for d in NEIGHBORS)
        next_position, next_direction = next((p, d) for p, d in neighbors if grid.get(p) == "#" and p != previous_position)
        movements.append(direction(current_direction, next_direction))
        current_direction = next_direction

    # move forward as much as possible
    steps = 0
    while grid.get(next_position := tuple(map(sum, zip(current_position, current_direction)))) == "#":
        steps += 1
        current_position, previous_position = next_position, current_position
    movements.append(str(steps))


routine_pattern = re.compile(r"^(?P<A>.{1,13})\1*(?P<B>.{1,13})(?:\1|\2)*(?P<C>.{1,13})(?:\1|\2|\3)*$")
routines_raw = routine_pattern.match("".join(movements)).groupdict()
routines = {f: [x for x in re.split(r"(\d+|L|R)", r) if x] for f, r in routines_raw.items()}
main = []
current_idx = 0
while movements[current_idx:]:
    for f, r in routines.items():
        if "".join(movements[current_idx:]).startswith("".join(r)):
            main.append(f)
            current_idx += len(r)
            break
    else:
        assert False, "Routines don't match up!"


def encode(routine):
    encoded = []
    for r in routine:
        if len(r) == 1:
            encoded.append(ord(r))
        else:
            encoded.extend([ord(r[0]), ord(r[1])])
        encoded.append(ord(","))
    encoded[-1] = ord("\n")
    return encoded


input_queue = deque()
input_queue.extend(encode(main))
input_queue.extend(encode(routines["A"]))
input_queue.extend(encode(routines["B"]))
input_queue.extend(encode(routines["C"]))
input_queue.extend(encode(["n"]))
robot = Intcode(program.copy(), inputs=input_queue, initial_memory={0: 2})
print(robot.run()[-1])