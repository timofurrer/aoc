from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

rucksacks = puzzle_input_raw.splitlines()
common = [(set(r[:len(r) // 2]) & set(r[len(r) // 2:])).pop() for r in rucksacks]
priorities = [ord(x) - 96 if x.islower() else ord(x) - 38 for x in common]
print(sum(priorities))