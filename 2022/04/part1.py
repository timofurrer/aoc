from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

def fully_contains(x, y): 
    """x is fully contained in y"""
    return x[0] >= y[0] and x[1] <= y[1]

pairs = [tuple(tuple(int(y) for y in x.split("-")) for x in line.split(",")) for line in puzzle_input_raw.splitlines()]
overlaps = sum(fully_contains(a, b) or fully_contains(b, a) for a, b in pairs)
print(overlaps)