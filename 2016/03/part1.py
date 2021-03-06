from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

triangles = [tuple(int(x) for x in x.split()) for x in puzzle_input_raw.splitlines()]

is_triangle = lambda a, b, c: a + b > c and b + c > a and c + a > b

valid_triangles = [t for t in triangles if is_triangle(*t)]
print(len(valid_triangles))