from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


elf_calories = [sum(int(c) for c in elf.splitlines()) for elf in puzzle_input_raw.split("\n\n")]

print(sum(sorted(elf_calories)[-3:]))