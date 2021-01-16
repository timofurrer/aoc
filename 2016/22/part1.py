from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import defaultdict
import re
import itertools

nodes_raw = [x.split() for x in puzzle_input_raw.splitlines()[2:]]
nodes = defaultdict()
for node, *specs in nodes_raw:
    x, y = tuple(int(x) for x in re.search(r"x(\d+)-y(\d+)", node).groups())
    nodes[(x, y)] = tuple(int(s[:-1]) for s in specs)

viable_pairs = sum(
    nodes[a][1] > 0 and nodes[a][1] <= nodes[b][2] 
    for a, b 
    in itertools.product(nodes.keys(), repeat=2)
)
print(viable_pairs)