from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

SUE_PATTERN = re.compile(r"(\w+): (\d+)")
sues = [dict(SUE_PATTERN.findall(x)) for x in puzzle_input_raw.splitlines()]
needle = dict([
    ("children", lambda x: x == "3"),
    ("cats", lambda x: int(x) > 7),
    ("samoyeds", lambda x: x == "2"),
    ("pomeranians", lambda x: int(x) < 3),
    ("akitas", lambda x: x == "0"),
    ("vizslas", lambda x: x == "0"),
    ("goldfish", lambda x: int(x) < 5),
    ("trees", lambda x: int(x) > 3),
    ("cars", lambda x: x == "2"),
    ("perfumes", lambda x: x == "1"),
])

matching_sue = next(s for s in sues if all(needle[k](v) for k, v in s.items()))
print(sues.index(matching_sue) + 1)