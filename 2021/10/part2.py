from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import functools

lines = [list(l) for l in puzzle_input_raw.splitlines()]

PAIRS = {"(": ")", "[": "]", "{": "}", "<": ">"}
POINTS = {")": 1, "]": 2, "}": 3, ">": 4}

completion_scores = []
for line in lines:
    chunk_stack = []
    for ch in line:
        if ch in PAIRS:
            chunk_stack.append(ch)
        elif PAIRS[chunk_stack.pop()] != ch:
            break
    else:
        if chunk_stack:
            completion_score = functools.reduce(
                lambda x, y: x * 5 + POINTS[y],
                (PAIRS[c] for c in reversed(chunk_stack)),
                0,
            )
            completion_scores.append(completion_score)


middle_score = sorted(completion_scores)[len(completion_scores) // 2]
print(middle_score)