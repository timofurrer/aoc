from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools

TOTAL_EGGNOG = 150

containers = [int(x) for x in puzzle_input_raw.splitlines()]
all_combinations = itertools.chain(*(itertools.combinations(containers, x) for x in range(3, len(containers) + 1)))
using_all_eggnog = [x for x in all_combinations if sum(x) == TOTAL_EGGNOG]
minimum_containers = min(using_all_eggnog, key=lambda x: len(x))
print(len([x for x in using_all_eggnog if len(x) == len(minimum_containers)]))