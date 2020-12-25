import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

cups = [int(x) for x in puzzle_input_raw] + list(range(10, 1_000_001))
cup_circle = dict(zip(cups, cups[1:]))
cup_circle[cups[-1]] = cups[0]
current_cup = cups[0]
max_cup = max(cups)


for _ in range(10_000_000):
    # pick up action
    picked_up = [cup_circle[current_cup]]
    for _ in range(2):
        picked_up.append(cup_circle[picked_up[-1]])

    # destination action
    destination = current_cup - 1
    while destination <= 0 or destination in picked_up:
        destination -= 1
        if destination <= 0:
            destination = max_cup

    # placing action
    cup_circle[current_cup] = cup_circle[picked_up[-1]]
    cup_circle[picked_up[-1]] = cup_circle[destination]
    cup_circle[destination] = picked_up[0]

    # select new current cup
    current_cup = cup_circle[current_cup]


c1 = cup_circle[1]
c2 = cup_circle[c1]
print(c1 * c2)