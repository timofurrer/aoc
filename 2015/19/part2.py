from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

_, medicine_molecule = puzzle_input_raw.split("\n\n")

# see https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/?utm_source=reddit&utm_medium=web2x&context=3
upper_chars = sum(x.upper() == x for x in medicine_molecule)
rn_extras = medicine_molecule.count("Rn")
ar_extras = medicine_molecule.count("Ar")
y_extras = medicine_molecule.count("Y")

print(upper_chars - rn_extras - ar_extras - 2 * y_extras -1 )