from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

PROCEDURE = re.compile(r"move (\d+) from (\d) to (\d)")

initial_stacks_raw, procedure = puzzle_input_raw.split("\n\n")

# Initial stack parsing
initial_stacks = initial_stacks_raw.splitlines()
stacks = {s: [] for s in initial_stacks[-1].split()}
for s in reversed(initial_stacks[:-1]):
    cs = s.replace("    ", " ").split(" ")
    for x, c in (x for x in enumerate(cs, start=1) if x[1]):
        stacks[str(x)].append(c[1:-1])

# Rearrangement Procedure
for amount, src, dst in (PROCEDURE.match(x).groups() for x in procedure.splitlines()):
    crates = stacks[src][-int(amount):]
    del stacks[src][-int(amount):]
    stacks[dst].extend(crates)

print("".join(c[-1] for c in stacks.values()))