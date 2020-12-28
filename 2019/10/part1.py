from pathlib import Path
import math

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()
    

asteroids_map = {(x, y): c == "#" for y, row in enumerate(puzzle_input_raw.splitlines()) for x, c in enumerate(row)}

def angle(source, target):
    result = math.atan2(target[0] - source[0], target[1] - source[1]) * 180 / math.pi
    return 360 + result if result < 0 else result

best = 0
for asteroid in (x for x, v in asteroids_map.items() if v):
    angles = {angle(asteroid, o) for o, v in asteroids_map.items() if v and o != asteroid}
    if len(angles) > best:
        best = len(angles)

print(best)