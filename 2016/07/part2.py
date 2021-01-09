from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re

ips = puzzle_input_raw.splitlines()
ips_separated_brackets = [(re.sub(r"\[([a-z]+)\]", "", x), re.findall(r"\[([a-z]+)\]", x)) for x in ips]

def does_support_ssl(supernet, hypernet):
    aba = [x for x in zip(supernet, supernet[1:], supernet[2:]) if x[0] == x[2] and x[0] != x[1]]
    if not aba:
        return False

    matching_bab = (b + a1 + b for a1, b, a2 in aba)
    return any(bab in n for bab in matching_bab for n in hypernet)

ips_with_ssl = [x for x, b in ips_separated_brackets if does_support_ssl(x, b)]
print(len(ips_with_ssl))