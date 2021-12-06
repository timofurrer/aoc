from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


from collections import Counter

fish = [int(x) for x in puzzle_input_raw.split(",")]
fish_pop = Counter(fish)


for _ in range(256):
    new = fish_pop[0]
    for i in range(8):
        fish_pop[i] = fish_pop[i + 1]
    fish_pop[6] += new
    fish_pop[8] = new


print(fish_pop.total())