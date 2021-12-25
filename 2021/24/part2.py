from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


code = [x.split() for x in puzzle_input_raw.splitlines()]

"""
 0 [SAME] inp w
 1 [SAME] mul x 0
 2 [SAME] add x z
 3 [SAME] mod x 26
 4 [UNIQ] div z 1  (1 or 26)
 5 [UNIQ] add x 13
 6 [SAME] eql x w
 7 [SAME] eql x 0
 8 [SAME] mul y 0
 9 [SAME] add y 25
10 [SAME] mul y x
11 [SAME] add y 1
12 [SAME] mul z y
13 [SAME] mul y 0
14 [SAME] add y w
15 [UNIQ] add y 15
16 [SAME] mul y x
17 [SAME] add z y
"""

INPUT_BLOCK_LENGTH = 18
UNIQUE_INSTRUCTION_LINES = [4, 5, 15]


def run(code: list, inputs: list[int]):
    stack = []
    for i in range(14):
        div, chk, add = map(int, (code[i * INPUT_BLOCK_LENGTH + x][-1] for x in UNIQUE_INSTRUCTION_LINES))
        if div == 1:
            stack.append((i, add))
        elif div == 26:
            j, add = stack.pop()
            inputs[i] = inputs[j] + add + chk
            if inputs[i] > 9:
                inputs[j] -= inputs[i] - 9
                inputs[i] = 9
            if inputs[i] < 1:
                inputs[j] += 1 - inputs[i]
                inputs[i] = 1

    return inputs


print("".join([str(x) for x in run(code, [1] * 14)]))