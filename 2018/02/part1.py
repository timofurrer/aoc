from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import Counter

box_ids = puzzle_input_raw.splitlines()

twos = 0
threes = 0
for occ in (Counter(b).values() for b in box_ids):
    twos += 2 in occ
    threes += 3 in occ

print(twos * threes)