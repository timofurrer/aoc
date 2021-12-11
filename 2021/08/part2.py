from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

from collections import deque

data = [tuple(tuple(set(d) for d in d.split()) for d in x.split("|")) for x in puzzle_input_raw.splitlines()]

UNIQUE_SEGMENTS_TO_DIGIT = {2: 1, 4: 4, 3: 7, 7: 8}

total_output_values_sum = 0
for signal_patterns, output_values in data:
    patterns = deque(signal_patterns)
    wire_map = {}
    while patterns:
        p = patterns.popleft()
        has_overlaps = lambda x, n: x in wire_map and len(wire_map[x] & p) == n
        if len(p) in UNIQUE_SEGMENTS_TO_DIGIT:  # matching unique signal pattern (1, 4, 7, 8)
            wire_map[UNIQUE_SEGMENTS_TO_DIGIT[len(p)]] = p
        elif len(p) == 5 and 2 not in wire_map and has_overlaps(4, 2):  # matching 2
            wire_map[2] = p
        elif len(p) == 5 and 3 not in wire_map and has_overlaps(1, 2):  # matching 3
            wire_map[3] = p
        elif len(p) == 5 and 5 not in wire_map and has_overlaps(4, 3) and has_overlaps(1, 1):  # matching 5
            wire_map[5] = p
        elif len(p) == 6 and 6 not in wire_map and has_overlaps(1, 1):  # matching 6
            wire_map[6] = p
        elif len(p) == 6 and 9 not in wire_map and has_overlaps(3, 5):  # matching 9
            wire_map[9] = p
        elif len(patterns) == 0:  # matching 0
            wire_map[0] = p
        else:
            patterns.append(p)

    output_values_sum = int("".join([next(str(k) for k, v in wire_map.items() if v == x) for x in output_values]))
    total_output_values_sum += output_values_sum

print(total_output_values_sum)