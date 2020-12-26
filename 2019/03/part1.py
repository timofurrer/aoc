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
    current = (0, 0)
    for direction, steps in wire:
        for d in [WIRE_DIRECTIONS[direction]] * steps:
            current = current[0] + d[0], current[1] + d[1]
            coords.add(current)
    return coords


wires = [[(x[0], int(x[1:])) for x in w.split(",")] for w in puzzle_input_raw.splitlines()]
intersections = set.intersection(*(coordinates_for_wire(w) for w in wires))
closest_intersections = min(abs(x[0]) + abs(x[1]) for x in intersections)
print(closest_intersections)