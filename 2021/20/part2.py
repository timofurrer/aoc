from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import functools

algorithm_raw, image_raw = puzzle_input_raw.split("\n\n")

algorithm = [int(p == "#") for x in algorithm_raw.splitlines() for p in x]
image = {(x, y): int(p == "#") for y, row in enumerate(image_raw.splitlines()) for x, p in enumerate(row)}

NEIGHBORS = [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

def enhancement(image: dict, outside_on: bool):
    start_x = min(x for (x, _), p in image.items() if p) - 1
    start_y = min(y for (_, y), p in image.items() if p) - 1
    end_x = max(x for (x, _), p in image.items() if p) + 2
    end_y = max(y for (_, y), p in image.items() if p) + 2
    return {
        (x, y): algorithm[int("".join([str(image.get(tuple(map(sum, zip((x, y), n))), int(outside_on))) for n in NEIGHBORS]), 2)]
        for x in range(start_x, end_x)
        for y in range(start_y, end_y)
    }


image = functools.reduce(lambda image, i: enhancement(image, i % 2 == 1), range(50), image)
print(sum(image.values()))