from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import itertools

original_password = puzzle_input_raw

contains_increasing_three = lambda w: any(ord(y) == ord(x) + 1 and ord(z) == ord(y) + 1 for x, y, z in zip(w, w[1:], w[2:]))
has_two_pairs = lambda w: sum(len("".join(g)) // 2 for _, g in itertools.groupby(w)) >= 2
is_suitable_password = lambda p: contains_increasing_three(p) and has_two_pairs(p)
ALPHABET = "abcdefghjkmnpqrstuvwxyz"  # alphabet without i, l and o
N = len(ALPHABET)
ALPHABET_INDEX = {d: i for i, d in enumerate(ALPHABET, 1)}

def b10_to_b26(num):
    if num == 0:
        return ""
    else:
        q, r = divmod(num - 1, N)
        return b10_to_b26(q) + ALPHABET[r]


def b26_to_b10(string):
    result = 0
    for char in string:
        result = result * N + ALPHABET_INDEX[char]
    return result

password_b26 = b26_to_b10(original_password)
suitable_password = next(p for p in (b10_to_b26(p) for p in range(password_b26, b26_to_b10("z" * 8))) if is_suitable_password(p))
print(suitable_password)