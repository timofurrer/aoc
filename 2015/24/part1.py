from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools
import math

packages = {int(x) for x in puzzle_input_raw.splitlines()}
packages_weight = sum(packages) // 3

first_group_size = 1
while first_group_size < len(packages) - 1:
    found = []
    for first_group in (frozenset(x) for x in itertools.combinations(packages, first_group_size) if sum(x) == packages_weight):
        second_group_size = first_group_size + 1
        match = None
        while second_group_size < len(packages) - first_group_size:
            if any(x for x in itertools.combinations(packages.difference(first_group), second_group_size) if sum(x) == sum(first_group) == sum(packages.difference(first_group).difference(x))):
                match = first_group
                break
            
            second_group_size += 1
        
        if match:
            found.append(match)

    if found:
        break
    first_group_size += 1

smallest_qe = min(found, key=lambda x: math.prod(x))
print(smallest_qe, math.prod(smallest_qe))