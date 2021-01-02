from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import hashlib

md5 = lambda x: hashlib.md5(x.encode("utf-8")).hexdigest()[:6]

for n in range(10_000_000):
    if all(x == "0" for x in md5(puzzle_input_raw + str(n))):
        print(n)
        break