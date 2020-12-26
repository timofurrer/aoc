import os
import sys
import math

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


tiles_raw = puzzle_input_raw.split(os.linesep + os.linesep)
tiles = {int(x.splitlines()[0][5:-1]): x.splitlines()[1:] for x in tiles_raw}


def edges(tile):
    top = tile[0]
    bottom = tile[-1]
    left = "".join(x[0] for x in tile)
    right = "".join(x[-1] for x in tile)
    return top, right, bottom, left


def transform_tile(tile):
    rotate = lambda t: list("".join(x[::-1]) for x in zip(*t))
    flip = lambda t: list(reversed(t.copy()))

    for _ in range(4):
        yield tile
        yield flip(tile)
        tile = rotate(tile)


def find_match(tile, tile_side, other_tiles):
    for other_tile in other_tiles.values():
        if tile == other_tile:
            continue

        for transformed_other_tile in transform_tile(other_tile):
            other_tile_side = (tile_side + 2) % 4
            if edges(tile)[tile_side] == edges(transformed_other_tile)[other_tile_side]:
                return transformed_other_tile


corners = [x for x, t in tiles.items() if sum(not find_match(t, s, tiles) for s in range(4)) == 2]
print(math.prod(corners))