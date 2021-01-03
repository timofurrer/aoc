from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools
import functools

ratings = {(x[0], x[-1][:-1]): int(x[3]) if x[2] == "gain" else int(x[3]) * -1 for x in (x.split() for x in puzzle_input_raw.splitlines())}
persons = set(itertools.chain(*(x for x in ratings)))

overall_happiness = {}
for seating in itertools.permutations(persons, len(persons)):
    table_seating = seating + (seating[0],) + seating[::-1]
    happiness = functools.reduce(lambda h, s: h + ratings[s], zip(table_seating, table_seating[1:]), 0)
    overall_happiness[seating] = happiness

maximal_happiness = max(overall_happiness.items(), key=lambda x: x[1])
print(maximal_happiness)