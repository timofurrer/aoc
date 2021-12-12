from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import defaultdict, deque, Counter

edges = [(a, b) for a, b in (line.split("-") for line in puzzle_input_raw.splitlines())]

# build graph
graph = defaultdict(list)
for f, t in edges:
    graph[f].append(t)
    graph[t].append(f)

# find all paths between `start` and `end` using BFS
START_NODE = "start"
END_NODE = "end"

visited_single_node_twice = lambda path: \
    (c := Counter(x for x in path if x.islower())) \
        and c.total() > len(c) + 1

stack = deque([(START_NODE, tuple())])
paths = set()
while stack:
    position, path = stack.popleft()
    if position in {START_NODE, END_NODE} and position in path:
        continue

    path = path + (position,)
    if visited_single_node_twice(path):
        continue

    if position == END_NODE:
        paths.add(path)
        continue

    stack.extend((x, path) for x in graph[position])

print(len(paths))