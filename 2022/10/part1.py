from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


xs = [1]
for cmd, *value in (l.split() for l in puzzle_input_raw.splitlines()):
    xs.append(xs[-1])
    if cmd == "addx":
        xs.append(xs[-1] + int(value[0]))


print(sum(c * xs[c - 1] for c in range(20, 260, 40)))