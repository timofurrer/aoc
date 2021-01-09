from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools

triangle_sheet = [tuple(int(x) for x in x.split()) for x in puzzle_input_raw.splitlines()]
triangle_sheet_T = list(map(tuple, zip(*triangle_sheet)))
triangles = list(itertools.chain(*(zip(*(iter(t),) * 3) for t in triangle_sheet_T)))

is_triangle = lambda a, b, c: a + b > c and b + c > a and c + a > b

valid_triangles = [t for t in triangles if is_triangle(*t)]
print(len(valid_triangles))