from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools

number = puzzle_input_raw

def look_and_say(look):
    return ''.join(str(len([1 for _ in v])) + k for k, v in itertools.groupby(look))

for _ in range(50):
    number = look_and_say(number)

print(len(number))