from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


heightmap = {(r, c): int(col) for r, row in enumerate(puzzle_input_raw.splitlines()) for c, col in enumerate(row)}

ADJACENT = [(0, -1), (1, 0), (0, 1), (-1, 0)]

low_points = sum(
    n + 1
    for p, n in heightmap.items()
    if all(n < heightmap.get(tuple(map(sum, zip(p, d))), 10) for d in ADJACENT)
)
print(low_points)