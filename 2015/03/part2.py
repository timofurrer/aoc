from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import defaultdict

MOVES = {"^": (0, 1), ">": (1 ,0), "v": (0, -1), "<": (-1, 0)}
santa_position = (0, 0)
robot_santa_position = (0, 0)
houses = defaultdict(int)

def pairwise(iterable):
    "s -> (s0, s1), (s2, s3), (s4, s5), ..."
    a = iter(iterable)
    return zip(a, a)

for santa_move, robot_santa_move in pairwise(puzzle_input_raw):
    houses[santa_position] += 1
    houses[robot_santa_position] += 1
    santa_position = tuple(map(sum, zip(santa_position, MOVES[santa_move])))
    robot_santa_position = tuple(map(sum, zip(robot_santa_position, MOVES[robot_santa_move])))

at_least_one_present = sum(p >= 1 for p in houses.values())
print(at_least_one_present)