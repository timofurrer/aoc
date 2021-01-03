from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

reindeers = {x[0]: (int(x[3]), int(x[6]), int(x[13])) for x in (x.split() for x in puzzle_input_raw.splitlines())}

seconds = 2_503
reindeer_states = {r: d for r, (_, d, _) in reindeers.items()}
distances = {r: 0 for r in reindeers}
points = {r: 0 for r in reindeers}

for second in range(1, seconds + 1):
    for reindeer, d in reindeers.items():
        if reindeer_states[reindeer] > 0:
            distances[reindeer] += d[0]
            reindeer_states[reindeer] -= 1

        if second % (d[1] + d[2]) == 0:
            reindeer_states[reindeer] = d[1]
    
    max_distance = max(distances.values())
    points.update({r: p + 1 for r, p in points.items() if distances[r] == max_distance})

winner = max(points.items(), key=lambda x: x[1])
print(winner)