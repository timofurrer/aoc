from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re

DISC_PATTERN = re.compile(r"has (\d+) positions; at time=0, it is at position (\d+).")

discs = [tuple(int(x) for x in DISC_PATTERN.search(d).groups()) for d in puzzle_input_raw.splitlines()]

previous_disc_period = 1
current_timestamp = 0
for disc_id, (positions, current_position) in enumerate(discs, start=1):
    # find matching period point for new disc
    while (current_timestamp + current_position + disc_id) % positions:
        current_timestamp += previous_disc_period
    # combine the `disc` with the previous discs to match up period
    previous_disc_period *= positions

print(current_timestamp)