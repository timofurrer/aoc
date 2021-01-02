from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with puzzle_input_path.open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

grid = {(x, y): s for y, row in enumerate(puzzle_input_raw.splitlines()) for x, s in enumerate(row)}
grid[(2, 2)] = "?"

NEIGHBORS = [(0, -1), (1, 0), (0, 1), (-1, 0)]
OUTER_NEIGHBORS = [(2, 1), (3, 2), (2, 3), (1, 2)]
INNER_NEIGHBORS = [[(x, 0) for x in range(5)], [(4, x) for x in range(5)], [(x, 4) for x in range(5)], [(0, x) for x in range(5)]]

def get_adjacent_bugs(coord, layer, outer_layer, inner_layer):
    neighbors_d = [n for n in NEIGHBORS if layer.get(tuple(map(sum, zip(coord, n))))]
    outer_neighbors = sum(outer_layer.get(OUTER_NEIGHBORS[NEIGHBORS.index(x)]) == "#" for x in set(NEIGHBORS).difference(neighbors_d))
    same_neighbors = sum(layer.get(c) == "#" for c in (tuple(map(sum, zip(coord, n))) for n in neighbors_d) if c != (2, 2))
    if coord in OUTER_NEIGHBORS:
        inner_neighbors = sum(inner_layer.get(c) == "#" for c in INNER_NEIGHBORS[OUTER_NEIGHBORS.index(coord)])
    else:
        inner_neighbors = 0
    
    neighbors = same_neighbors + outer_neighbors + inner_neighbors
    return neighbors

MINUTES = 200
create_new_layer = lambda: {(x, y): "?" if (x, y) == (2, 2) else "." for y in range(5) for x in range(5)}
layers = [create_new_layer() for _ in range(MINUTES + 1)]
layers[len(layers) // 2] = grid

for _ in range(MINUTES):
    new_layers = [None for _ in range(len(layers))]

    for layer_id, layer in enumerate(layers):
        outer_layer = layers[layer_id - 1] if layer_id > 0 else create_new_layer()
        inner_layer = layers[layer_id + 1] if layer_id + 1 < len(layers) else create_new_layer()

        new_layer = {}
        for coord, state in layer.items():
            if state == "?":
                new_layer[coord] = "?"
                continue

            adjacent_bugs = get_adjacent_bugs(coord, layer, outer_layer, inner_layer)
            if state == "#" and adjacent_bugs != 1:
                new_layer[coord] = "."
            elif state == "." and adjacent_bugs in {1, 2}:
                new_layer[coord] = "#"
            else:
                new_layer[coord] = state
            
        new_layers[layer_id] = new_layer
    layers = new_layers

bugs = sum(x == "#" for layer in layers for x in layer.values())
print(bugs)