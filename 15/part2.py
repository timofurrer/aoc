import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


def play(start_numbers):
    last = start_numbers[-1]
    spoken = {x: i for i, x in enumerate(start_numbers[:-1], start=1)}
    for turn in range(len(start_numbers), 30000000):
        last_turn = spoken.get(last, turn)
        spoken[last] = turn
        last = turn - last_turn
    return last


start_numbers = [int(x) for x in puzzle_input_raw.split(",")]
print(play(start_numbers))