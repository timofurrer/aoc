from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

SUE_PATTERN = re.compile(r"(\w+): (\d+)")
sues = [dict(SUE_PATTERN.findall(x)) for x in puzzle_input_raw.splitlines()]
needle = dict([
    ("children", "3"),
    ("cats", "7"),
    ("samoyeds", "2"),
    ("pomeranians", "3"),
    ("akitas", "0"),
    ("vizslas", "0"),
    ("goldfish", "5"),
    ("trees", "3"),
    ("cars", "2"),
    ("perfumes", "1"),
])

matching_sue = next(s for s in sues if all(needle[k] == v for k, v in s.items()))
print(sues.index(matching_sue) + 1)