from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

def overlap(x, y): 
    """x is fully contained in y"""
    return bool(set(range(x[0], x[1] + 1)) & set(range(y[0], y[1] + 1)))

pairs = [tuple(tuple(int(y) for y in x.split("-")) for x in line.split(",")) for line in puzzle_input_raw.splitlines()]
overlaps = sum(overlap(a, b) for a, b in pairs)
print(overlaps)