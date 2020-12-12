import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


def rotate_right(x, y, times):
    x, y = y, x * -1
    return rotate_right(x, y, times - 1) if times > 1 else (x, y)

def rotate_left(x, y, times):
    x, y = y * -1, x
    return rotate_left(x, y, times - 1) if times > 1 else (x, y)


instructions = {
    "N": lambda w_x, w_y, x, y, arg: (w_x, w_y + arg, x, y),
    "S": lambda w_x, w_y, x, y, arg: (w_x, w_y - arg, x, y),
    "E": lambda w_x, w_y, x, y, arg: (w_x + arg, w_y, x, y),
    "W": lambda w_x, w_y, x, y, arg: (w_x - arg, w_y, x, y),
    "L": lambda w_x, w_y, x, y, arg: (*rotate_left(w_x, w_y, arg // 90), x, y),
    "R": lambda w_x, w_y, x, y, arg: (*rotate_right(w_x, w_y, arg // 90), x, y),
    "F": lambda w_x, w_y, x, y, arg: (w_x, w_y, x + w_x * arg, y + w_y * arg),
}

w_x, w_y, x, y = 10, 1, 0, 0
for instr, arg in ((x[:1], int(x[1:])) for x in puzzle_input_raw.splitlines()):
    w_x, w_y, x, y = instructions[instr](w_x, w_y, x, y, arg)

distance = abs(x) + abs(y)
print(distance)