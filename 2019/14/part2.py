from pathlib import Path
import re
import math
from collections import defaultdict
from queue import Queue

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

reactions = []

for raw_reaction in puzzle_input_raw.splitlines():
    sources, target = raw_reaction.split("=>", maxsplit=2)
    sources = [tuple(int(x) if x.isdigit() else x for x in x.strip().split()) for x in sources.split(",")]
    target = tuple(int(x) if x.isdigit() else x for x in target.strip().split())
    reactions.append((sources, target))


def calculate_ore_per_fuel(required_fuel):
    leftovers = defaultdict(int)
    reactions_needed = Queue()
    reactions_needed.put(("FUEL", required_fuel))
    ore = 0

    while not reactions_needed.empty():
        chemical, amount = reactions_needed.get()

        if chemical == "ORE":
            ore += amount
            continue

        if leftovers[chemical] >= amount:
            leftovers[chemical] -= amount
            continue

        reaction = next(r for r in reactions if r[1][1] == chemical)
        needed = amount - leftovers[chemical]
        multiplier = math.ceil(needed / reaction[1][0])
        leftover = multiplier * reaction[1][0] - needed
        for source_amount, source_chemical in reaction[0]:
            reactions_needed.put((source_chemical, source_amount * multiplier)) 
        leftovers[chemical] = leftover

    return ore

max_fuel = 0
fuel = 1_766_100   # brute forced
while True:
    ore = calculate_ore_per_fuel(fuel)
    if ore <= 1_000_000_000_000:
        max_fuel = fuel
    else:
        break
    fuel += 1

print(max_fuel)