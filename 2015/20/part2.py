from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import defaultdict

limit = 1_000_000
houses = defaultdict(int)
number = int(puzzle_input_raw)

for elf in range(1, number):
    for house in range(elf, min(elf * 50 + 1, limit), elf):
        houses[house] += 11 * elf
        
    if houses[elf] >= number:
        print(elf)
        break