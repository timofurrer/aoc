import os
import sys
import math

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


tiles_raw = puzzle_input_raw.split(os.linesep + os.linesep)
tiles = {int(x.splitlines()[0][5:-1]): x.splitlines()[1:] for x in tiles_raw}


SEA_MONSTER = [
    "                  # ", 
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
]
SEA_MONSTER_COORDS = [
    (x, y)
    for y, row in enumerate(SEA_MONSTER)
    for x, col in enumerate(row)
    if col == "#"
]


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
    for x, other_tile in other_tiles.items():
        if tile in transform_tile(other_tile):
            continue

        for transformed_other_tile in transform_tile(other_tile):
            other_tile_side = (tile_side + 2) % 4
            if edges(tile)[tile_side] == edges(transformed_other_tile)[other_tile_side]:
                return transformed_other_tile


corners = [x for x, t in tiles.items() if sum(not find_match(t, s, tiles) for s in range(4)) == 2]

GRID_SIZE = int(math.sqrt(len(tiles)))
# get top left corner tile
top_left_corner = next(
    t 
    for t 
    in transform_tile(tiles[corners[0]]) 
    if find_match(t, 1, tiles) and find_match(t, 2, tiles)
)

def find_row(start, tiles):
    row = [start]
    for _ in range(GRID_SIZE - 1):
        row.append(find_match(row[-1], 1, tiles))
    return row

# create image grid
grid = []
start = top_left_corner
for _ in range(GRID_SIZE):
    row = find_row(start, tiles)
    grid.append(row)
    start = find_match(row[0], 2, tiles)

image = ["".join(x[1:-1] for x in scan_line) for row in grid for scan_line in zip(*(x[1:-1] for x in row))]
total_roughness = sum(r.count("#") for r in image)
for potential_image in transform_tile(image):
    # count monsters
    sea_monsters = sum(
        all(potential_image[x + mx][y + my] == "#" for mx, my in SEA_MONSTER_COORDS)
        for y in range(len(potential_image) - len(SEA_MONSTER))
        for x in range(len(potential_image) - len(SEA_MONSTER[1]))
    )

    if sea_monsters:
        roughness = total_roughness - sea_monsters * len(SEA_MONSTER_COORDS)
        print(roughness)
        break