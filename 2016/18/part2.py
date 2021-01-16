from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

row = [x == "." for x in puzzle_input_raw]

is_trap = lambda left, center, right: not (
       (not left and not center and right)
    or (not center and not right and left)
    or (not left and center and right)
    or (not right and center and left)
)

safe_tiles = sum(row)
for _ in range(400_000 - 1):
    x = [True] + row + [True]
    row = [is_trap(*r) for r in zip(x, x[1:], x[2:])]
    safe_tiles += sum(row)

print(safe_tiles)