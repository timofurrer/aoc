import os
import sys
from collections import Counter, defaultdict

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


adapters = sorted(int(x) for x in puzzle_input_raw.splitlines())
adapters.append(adapters[-1] + 3)

# This is the Climbing Stairs Problem, but with 1, 2 or 3 steps ;)
# F(N) = F(N - 1) + F(N - 2) + F(N - 3)
ways_to_joltage = defaultdict(int)
ways_to_joltage[0] = 1
for n in adapters:
    ways_to_joltage[n] = ways_to_joltage[n - 1] + ways_to_joltage[n - 2] + ways_to_joltage[n - 3]

joltage_to_device = ways_to_joltage[adapters[-1]]
print(joltage_to_device)

# Another way to solve this (and by building on puzzle 1) would be to count the permutations 
# of 1s between the adapters with 3 joltage differences. 