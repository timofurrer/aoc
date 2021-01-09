from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re

ips = puzzle_input_raw.splitlines()
ips_separated_brackets = [(x, re.findall(r"\[([a-z]+)\]", x)) for x in ips]

has_abba = lambda x: any(x[0] == x[3] and x[1] == x[2] and x[0] != x[1] for x in zip(x, x[1:], x[2:], x[3:]))
is_valid = lambda x, b: has_abba(x) and not any(has_abba(y) for y in b) 

valid_ips = [x for x, b in ips_separated_brackets if is_valid(x, b)]
print(len(valid_ips))