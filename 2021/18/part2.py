from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import math
import functools
import itertools

snailfish_numbers = [l for l in puzzle_input_raw.splitlines()]


def parse(raw_snailfish_number: str) -> list:
    parsed = []
    level = 0
    for c in raw_snailfish_number:
        if c == "[":
            level += 1
        elif c == "]":
            level -= 1
        elif c != ',':
            parsed.append((int(c), level))
    return parsed


def explode(number: list) -> bool:
    for i, (n, l) in (x for x in enumerate(number) if x[1][1] >= 5):
        # the pair's left value is added to the first regular number to the left of the exploding pair (if any)
        if i > 0:
            number[i - 1] = (number[i - 1][0] + n, number[i - 1][1])

        # the pair's right value is added to the first regular number to the right of the exploding pair (if any)
        if i + 2 < len(number):
            number[i + 2] = (number[i + 2][0] + number[i + 1][0], number[i + 2][1])

        # the pair is replaced with zero
        number[i:i + 2] = [(0, l - 1)]
        return True

    return False


def split(numbers: list) -> bool:
    for i, (n, l) in (x for x in enumerate(numbers) if x[1][0] >= 10):
        left = n // 2
        right = math.ceil(n / 2)
        numbers[i:i + 1] = [(left, l + 1), (right, l + 1)]
        return True

    return False


def add(a: list, b: list) -> list:
    added = [(n, l + 1) for n, l in a + b]
    while explode(added) or split(added):
        ...
    return added


def magnitude(number):
    while len(number) > 1:
        for i in range(len(number)-1):
            if number[i][1] == number[i + 1][1]:
                number[i:i+2] = [(3*number[i][0]+2*number[i+1][0], number[i][1]-1)]
                break

    return number[0][0]


number = functools.reduce(add, (parse(l) for l in snailfish_numbers))

max_mag = 0
for a, b in itertools.combinations(snailfish_numbers, 2):
    max_mag = max(max_mag, magnitude(add(parse(a), parse(b))), magnitude(add(parse(b), parse(a))))

print(max_mag)