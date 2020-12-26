import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


directions = {0: "E", 90: "S", 180: "W", 270: "N"}
instructions = {
    "N": lambda x, y, d, arg: (x, y + arg, d),
    "S": lambda x, y, d, arg: (x, y - arg, d),
    "E": lambda x, y, d, arg: (x + arg, y, d),
    "W": lambda x, y, d, arg: (x - arg, y, d),
    "L": lambda x, y, d, arg: (x, y, (d - arg) % 360),
    "R": lambda x, y, d, arg: (x, y, (d + arg) % 360),
}
instructions["F"] = lambda x, y, d, arg: instructions[directions[d]](x, y, d, arg)

x, y, d = 0, 0, 0
for instr, arg in ((x[:1], int(x[1:])) for x in puzzle_input_raw.splitlines()):
    x, y, d = instructions[instr](x, y, d, arg)

distance = abs(x) + abs(y)
print(distance)