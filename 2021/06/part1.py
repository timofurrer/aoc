from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


fish = [int(x) for x in puzzle_input_raw.split(",")]


for i in range(80):
    new_fish = []
    for fi in range(len(fish)):
        f = fish[fi]
        if f == 0:
            fish[fi] = 6
            new_fish.append(8)
        else:
            fish[fi] -= 1
    fish.extend(new_fish)

print(len(fish))