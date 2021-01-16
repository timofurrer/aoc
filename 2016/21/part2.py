from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from collections import deque

SWAP_POS_PATTERN = re.compile(r"swap position (\d) with position (\d)")
SWAP_LET_PATTERN = re.compile(r"swap letter ([a-z]) with letter ([a-z])")
ROTATE_STEPS = re.compile(r"rotate (left|right) (\d+) steps?")
ROTATE_POS_STEPS = re.compile(r"rotate based on position of letter ([a-z])")
REVERSE_POS = re.compile(r"reverse positions (\d) through (\d)")
MOVE_POS = re.compile(r"move position (\d) to position (\d)")

password = list("fbgdceah")

for instruction in reversed(puzzle_input_raw.splitlines()):
    if instruction.startswith("swap position"):
        y, x = tuple(int(x) for x in SWAP_POS_PATTERN.match(instruction).groups())
        password[x], password[y] = password[y], password[x]
    elif instruction.startswith("swap letter"):
        y, x = (password.index(x) for x in SWAP_LET_PATTERN.match(instruction).groups())
        password[x], password[y] = password[y], password[x]
    elif instruction.startswith("rotate"):
        if instruction.startswith("rotate based"):
            a = ROTATE_POS_STEPS.match(instruction).groups()[0]
            rx = password.index(a)
            # only works for 8 chars - reverse engineered
            r = [1, 1, 6, 2, 7, 3, 0, 4][rx]
        else:
            d, x = ROTATE_STEPS.match(instruction).groups()
            r = int(x) * -1 if d == "left" else int(x)
        tmp = deque(password)
        tmp.rotate(r * - 1)
        password = list(tmp)
    elif instruction.startswith("reverse"):
        x, y = tuple(int(x) for x in REVERSE_POS.match(instruction).groups())
        password = password[:x] + list(reversed(password[x:y + 1])) + password[y + 1:]
    elif instruction.startswith("move"):
        y, x = tuple(int(x) for x in MOVE_POS.match(instruction).groups())
        a = password.pop(x)
        password.insert(y, a)
    else:
        assert False

print("".join(password))