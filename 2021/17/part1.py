from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import re

target_area = tuple(
    int(x)
    for x in
    re.findall(r"-?\d+", puzzle_input_raw)
)
is_in_target_area = lambda x, y: (
    target_area[0] <= x <= target_area[1]
    and target_area[2] <= y <= target_area[3]
)
not_overshot_target_area = lambda x, y: x <= target_area[1] and target_area[2] <= y


def trajectory_highest_point(velocity):
    (x, y), t = (0, 0), 0
    while not_overshot_target_area(x, y):
        x, y = (x + velocity[0], y + velocity[1])
        velocity = max(0, velocity[0] - 1), velocity[1] - 1
        t = max(t, y)
        if is_in_target_area(x, y):
            return t
    return None


velocities = {
    (x, y): t
    for x in range(0, target_area[1] + 1)
    for y in range(target_area[2], -target_area[2])
    if (t := trajectory_highest_point((x, y))) is not None
}

print(max(velocities.values()))