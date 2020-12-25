import os
import sys

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

card_pub_key, door_pub_key = [int(x) for x in puzzle_input_raw.splitlines()]

SUBJECT_NUMBER = 7
DIVIDER = 20201227


def brute_force_loop_size(pub_key, subject_number=SUBJECT_NUMBER):
    value = 1
    loop_size = 0
    while value != pub_key:
        value *= subject_number
        value %= DIVIDER
        loop_size += 1
    return loop_size


def calc_encryption_key(pub_key, loop_size):
    value = 1
    for _ in range(loop_size):
        value *= pub_key
        value %= DIVIDER
    return value


card_loop_size = brute_force_loop_size(card_pub_key)
door_loop_size = brute_force_loop_size(door_pub_key)
encryption_key = calc_encryption_key(card_pub_key, door_loop_size)
print(encryption_key)