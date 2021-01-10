from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import hashlib
import itertools
from collections import defaultdict

first_triplet = lambda h: next((x for x, y, z in zip(h, h[1:], h[2:]) if x == y == z), None)
first_quintuplet = lambda h: next((a for a, b, c, d, e in zip(h, h[1:], h[2:], h[3:], h[4:]) if a == b == c == d == e), None)


def md5(i):
    m = hashlib.md5((puzzle_input_raw + str(i)).encode("utf-8")).hexdigest()
    for _ in range(2016):
        m = hashlib.md5(m.encode("utf-8")).hexdigest()
    return m


keys = []
require_quintuplet = defaultdict(list)
idx = 0

while len(keys) < 64 or (idx - keys[-1][0]) <= 1_000:
    current_hash = md5(idx)

    if quintuplet := first_quintuplet(current_hash):
        for q_idx, q_hash in require_quintuplet[quintuplet]:
            if (idx - q_idx) <= 1_000:
                keys.append((q_idx, q_hash))
        keys.sort()
        require_quintuplet[quintuplet].clear()

    if triplet := first_triplet(current_hash):
        require_quintuplet[triplet].append((idx, current_hash))

    idx += 1

print(keys[63])