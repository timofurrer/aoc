from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

pair_twice = lambda w: re.search(r"(..).*\1", w) is not None
another_enclosed = lambda w: any(x == z and x != y for x, y, z in zip(w, w[1:], w[2:]))

is_nice = lambda w: pair_twice(w) and another_enclosed(w)

nice = sum(is_nice(w) for w in puzzle_input_raw.splitlines())
print(nice)