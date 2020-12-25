import os
import sys
from collections import deque

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

cup_circle = [int(x) for x in puzzle_input_raw]
current_cup = cup_circle[0]

# optimization is in part 2 ;)
for _ in range(100):
    cup_location = cup_circle.index(current_cup)
    picked_up = [cup_circle[i % len(cup_circle)] for i in range(cup_location + 1, cup_location + 4)]
    list(map(cup_circle.remove, picked_up))
    destination = sorted((x for x in cup_circle if x < current_cup), key=lambda x: current_cup - x)
    if destination:
        destination = destination[0]
    else:
        destination = max(x for x in cup_circle if x != current_cup)
    destination_location = cup_circle.index(destination)
    cup_circle = cup_circle[:destination_location + 1] + picked_up + cup_circle[destination_location + 1:]
    current_cup = cup_circle[(cup_circle.index(current_cup) + 1) % len(cup_circle)]


cup_string = deque(cup_circle)
cup_string.rotate(len(cup_circle) - cup_circle.index(1))
print("".join(str(x) for x in list(cup_string)[1:]))