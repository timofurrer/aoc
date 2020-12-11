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


def next_seat_in_line(seats, seat_x, seat_y, direction):
    def move(x, y):
        while True:
            x += direction[0]
            y += direction[1]
            yield x, y

    return next(seats.get(coord) for coord in move(seat_x, seat_y) if seats.get(coord) != ".")


# Line direction movement coords
directions = [
    (-1, -1), (0, -1), (1, -1),
    (-1, 0),           (1, 0),
    (-1, 1),  (0, 1),  (1, 1)
]

changed = True
while changed:
    changed = False
    changed_seats = {}

    for (x, y), seat in seats.items():
        neighbor_fields = [next_seat_in_line(seats, x, y, direction) for direction in directions]
        occupied_neighbor_seats = neighbor_fields.count('#')

        # rules
        if seat == "L" and occupied_neighbor_seats == 0:
            changed_seats[(x, y)] = "#"
            changed = True
        elif seat == "#" and occupied_neighbor_seats >= 5:
            changed_seats[(x, y)] = "L"
            changed = True
        else:
            changed_seats[(x, y)] = seat

    seats = changed_seats


stable_occupied_seats = list(seats.values()).count("#")
print(stable_occupied_seats)