from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

lines = [list(l) for l in puzzle_input_raw.splitlines()]

PAIRS = {"(": ")", "[": "]", "{": "}", "<": ">"}
POINTS = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137
}

corrupted = []
for line in lines:
    chunk_stack = []
    for ch in line:
        if ch in PAIRS:
            chunk_stack.append(ch)
        elif PAIRS[chunk_stack.pop()] != ch:
            corrupted.append(ch)
            break

error_score = sum(POINTS[c] for c in corrupted)
print(error_score)