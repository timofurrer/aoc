from pathlib import Path
import re
import itertools

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

moons_coords = [tuple(int(x) for x in re.findall(r"\w=([0-9-]+)", m)) for m in puzzle_input_raw.splitlines()]
moons_velocities = [(0, 0, 0) for _ in range(len(moons_coords))]

cmp = lambda x, y: (x > y) - (x < y)
apply = lambda x, y: tuple(map(sum, zip(x, y)))
apply_list = lambda x, y: [apply(c, v) for c, v in zip(x, y)]


def time_step(moons_coords, moons_velocities):
    # apply gravity
    for (moon_idx, moon_coords), (other_moon_idx, other_moon_coords) in itertools.combinations(enumerate(moons_coords), 2):
        gravity_delta = tuple(itertools.starmap(cmp, zip(moon_coords, other_moon_coords)))
        moons_velocities[moon_idx] = apply(moons_velocities[moon_idx], tuple(x * -1 for x in gravity_delta))
        moons_velocities[other_moon_idx] = apply(moons_velocities[other_moon_idx], gravity_delta)

    # apply velocity
    return apply_list(moons_coords, moons_velocities), moons_velocities


for _ in range(1_000):
    moons_coords, moons_velocities = time_step(moons_coords, moons_velocities)

e_pots = [sum(map(abs, m)) for m in moons_coords]
e_kins = [sum(map(abs, v)) for v in moons_velocities]
e_tot = sum(p * k for p, k in zip(e_pots, e_kins))
print(e_tot)