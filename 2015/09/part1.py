from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools
import functools

flights = {frozenset([x.split()[0], x.split()[2]]): int(x.split()[-1]) for x in puzzle_input_raw.splitlines()}
destinations = set(itertools.chain(*{x for x in flights}))

route_distances = {}
for route in itertools.permutations(destinations, len(destinations)):
    distance = functools.reduce(lambda d, f: d + flights[frozenset(f)], zip(route, route[1:]), 0)
    route_distances[route] = distance

shortest_route = min(route_distances.items(), key=lambda x: x[1])
print(shortest_route)