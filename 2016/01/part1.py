from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

path = [(x[0], int(x[1:])) for x in puzzle_input_raw.split(", ")]

current_direction = (1, 0)
current_position = (0, 0)

TURN = {
    "L": lambda d: (-d[1], d[0]),
    "R": lambda d: (d[1], -d[0]),
}

for turn, forward in path:
    current_direction = TURN[turn](current_direction)
    current_position = tuple(map(sum, zip(current_position, (x * forward for x in current_direction))))

print(sum(abs(x) for x in current_position))