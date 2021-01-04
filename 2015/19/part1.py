from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re

replacements_raw, medicine_molecule = puzzle_input_raw.split("\n\n")
replacements = [tuple(x.strip() for x in x.split("=>")) for x in replacements_raw.splitlines()]

possible_molecules = []
for replacement in replacements:
    for m in re.finditer(re.escape(replacement[0]), medicine_molecule):
        possible_molecules.append(medicine_molecule[:m.start()] + replacement[1] + medicine_molecule[m.end():])
    
print(len(set(possible_molecules)))