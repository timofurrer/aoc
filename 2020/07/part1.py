import os
import sys
import re
from dataclasses import dataclass, field

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


class FactoryDict(dict):
    def __init__(self, factory):
        self.factory = factory

    def __missing__(self, key):
        self[key] = self.factory(key)
        return self[key]


@dataclass
class Bag:
    color: str
    can_contain: list = field(default_factory=lambda: [], compare=False)

    def __contains__(self, other):
        return other in self.can_contain or any(other in b for b in self.can_contain)


# parse puzzle input
bags = FactoryDict(lambda x: Bag(x))
bag_pattern = re.compile(r"(?:\d+) ((?:\w+) (?:\w+)) bag")
for line in puzzle_input_raw.splitlines():
    bag = bags[" ".join(line.split()[:2])]

    if "no other bags" in line:
        continue

    bag.can_contain.extend(bags[b] for b in bag_pattern.findall(line))


# traverse bag network to find bag
needle = Bag("shiny gold")
can_contain_needle = [b for b in bags.values() if needle in b]
print(len(can_contain_needle))