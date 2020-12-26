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

    def required_space(self):
        return 1 + sum(a * b.required_space() for a, b in self.can_contain)


# parse puzzle input
bags = FactoryDict(lambda x: Bag(x))
bag_pattern = re.compile(r"(\d+) ((?:\w+) (?:\w+)) bag")
for line in puzzle_input_raw.splitlines():
    bag = bags[" ".join(line.split()[:2])]

    if "no other bags" in line:
        continue

    bag.can_contain.extend((int(a), bags[b]) for a, b in bag_pattern.findall(line))


# count containing bags
required_space = bags["shiny gold"].required_space() - 1  # the `-1` is for @Elias
print(required_space)