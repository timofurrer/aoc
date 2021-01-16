from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

rows = [[x == "." for x in puzzle_input_raw]]

is_trap = lambda left, center, right: not (
       (not left and not center and right)
    or (not center and not right and left)
    or (not left and center and right)
    or (not right and center and left)
)

for _ in range(40 - 1):
    x = [True] + rows[-1] + [True]
    rows.append([is_trap(*r) for r in zip(x, x[1:], x[2:])])

print(sum(sum(r) for r in rows))