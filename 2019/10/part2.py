from pathlib import Path
import math

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()
    

asteroids_map = {(x, y): c == "#" for y, row in enumerate(puzzle_input_raw.splitlines()) for x, c in enumerate(row)}

def angle(source, target):
    result = math.atan2(target[0] - source[0], source[1] - target[1]) * 180 / math.pi
    return (360 + result) if result < 0 else result

asteroid_scores = {}
for asteroid in (x for x, v in asteroids_map.items() if v):
    angles = {angle(asteroid, o) for o, v in asteroids_map.items() if v and o != asteroid}
    asteroid_scores[asteroid] = angles

station_coords, _ = max(asteroid_scores.items(), key=lambda x: len(x[1]))
asteroids = sorted(
    ((a, angle(station_coords, a)) for a, v in asteroids_map.items() if v and a != station_coords),
    key=lambda x: (x[1], abs(station_coords[0] - x[0][0]) + abs(station_coords[1] - x[0][1]))
)

idx = 0
nth = 1
last = asteroids.pop(idx)

while nth < 200:
    if last[1] == asteroids[idx][1]:
        idx += 1
        continue

    last = asteroids.pop(idx)
    nth += 1

print(f"Vaporized 200th: {last[0][0] * 100 + last[0][1]}")