from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

octopus_grid = {(x, y): int(l) for y, r in enumerate(puzzle_input_raw.splitlines()) for x, l in enumerate(r)}

ADJACENT_DELTAS = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

flashes = 0
for step in range(100):
    # First, the energy level of each octopus increases by 1.
    octopus_grid = {c: l + 1 for c, l in octopus_grid.items()}
    # Then, any octopus with an energy level greater than 9 flashes.
    octopus_flashed = set()
    while any(l > 9 for l in octopus_grid.values()):
        # get flashing
        flashing_octopus = {o for o, l in octopus_grid.items() if l > 9}
        # set flashing to 0 and mark as flashed
        octopus_grid = {c: 0 if c in flashing_octopus else l for c, l in octopus_grid.items()}
        octopus_flashed.update(flashing_octopus)
        # increase adjacent octopus
        for adjacent_octopus in (tuple(map(sum, zip(o, d))) for o in flashing_octopus for d in ADJACENT_DELTAS):
            if adjacent_octopus in octopus_grid and adjacent_octopus not in octopus_flashed:
                octopus_grid[adjacent_octopus] += 1

    flashes += len(octopus_flashed)

print(flashes)