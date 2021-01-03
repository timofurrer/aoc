from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

reindeers = {x[0]: (int(x[3]), int(x[6]), int(x[13])) for x in (x.split() for x in puzzle_input_raw.splitlines())}

seconds = 2_503
distances = {}
for reindeer, d in reindeers.items():
    p, r = divmod(seconds, d[1] + d[2])
    x = p * (d[0] * d[1]) + min(r, d[1]) * d[0]
    distances[reindeer] = x

winner = max(distances.items(), key=lambda x: x[1])
print(winner)