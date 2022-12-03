from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

rucksacks = puzzle_input_raw.splitlines()
groups = [(set(r) for r in rucksacks[i:i + 3]) for i in range(0, len(rucksacks), 3)]
common = [(a & b & c).pop() for a, b, c in groups]
priorities = [ord(x) - 96 if x.islower() else ord(x) - 38 for x in common]
print(sum(priorities))