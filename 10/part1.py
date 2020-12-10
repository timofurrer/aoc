import os
import sys
from collections import Counter

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

adapters = sorted(int(x) for x in puzzle_input_raw.splitlines())
# add beginning and end to adapters
adapters.insert(0, 0)
adapters.append(adapters[-1] + 3)

diff_counter = Counter(y - x for x, y in zip(adapters, adapters[1:]))
result = diff_counter[1] * diff_counter[3]
print(result)