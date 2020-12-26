import os
import sys
import copy

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# parse input
seats = {}
for y, row in enumerate(puzzle_input_raw.splitlines()):
    for x, col in enumerate(row):
        seats[(x, y)] = col


changed = True
while changed:
    changed = False
    changed_seats = {}

    for (x, y), seat in seats.items():
        adjacent_seat_coords = [
            (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
            (x - 1, y),                 (x + 1, y),
            (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
        ]
        neighbor_fields = [seats.get(n) for n in adjacent_seat_coords]
        occupied_neighbor_seats = neighbor_fields.count("#")

        # rules
        if seat == "L" and occupied_neighbor_seats == 0:
            changed_seats[(x, y)] = "#"
            changed = True
        elif seat == "#" and occupied_neighbor_seats >= 4:
            changed_seats[(x, y)] = "L"
            changed = True
        else:
            changed_seats[(x, y)] = seat

    seats = changed_seats


stable_occupied_seats = list(seats.values()).count("#")
print(stable_occupied_seats)