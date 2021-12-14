from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import itertools
from collections import defaultdict

template, pair_insertion_rules_raw = puzzle_input_raw.split("\n\n")
pair_insertion_rules = {}
for rule, insertion in (x.split("->") for x in pair_insertion_rules_raw.splitlines()):
    insertion = insertion.strip()
    pair_insertion_rules[rule.strip()] = (rule[0] + insertion, insertion + rule[1])

counterdict = defaultdict(int)
for pair in itertools.pairwise(template):
    counterdict["".join(pair)] += 1


def step(counterdict):
    new_counterdict = defaultdict(int)
    for ins_pair, count in ((ins_pair, count) for pair, count in counterdict.items() for ins_pair in pair_insertion_rules[pair]):
        new_counterdict[ins_pair] += count
    return new_counterdict


for _ in range(40):
    counterdict = step(counterdict)


counter = defaultdict(int, {template[0]: 1, template[-1]: 1})
for (c1, c2), v in counterdict.items():
    counter[c1] += v
    counter[c2] += v
sorted_counter = sorted(((k, v // 2) for k, v in counter.items()), key=lambda x: x[1], reverse=True)

print(sorted_counter[0][1] - sorted_counter[-1][1])