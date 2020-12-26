from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


WIRE_DIRECTIONS = {
    "U": (0, 1),
    "L": (1, 0),
    "D": (0, -1),
    "R": (-1, 0),
}


def coordinates_for_wire(wire):
    coords = set()
    coord_steps = {}
    current = (0, 0)
    total_steps = 0
    for direction, steps in wire:
        for d in [WIRE_DIRECTIONS[direction]] * steps:
            current = current[0] + d[0], current[1] + d[1]
            total_steps += 1
            coords.add(current)
            if current not in coord_steps:
                coord_steps[current] = total_steps
    return coords, coord_steps


wires = [[(x[0], int(x[1:])) for x in w.split(",")] for w in puzzle_input_raw.splitlines()]
wire_coords_and_steps = [coordinates_for_wire(w) for w in wires]
intersections = set.intersection(*(i for i, _ in wire_coords_and_steps))
fewest_steps = min(sum(s[1][i] for s in wire_coords_and_steps) for i in intersections)
print(fewest_steps)