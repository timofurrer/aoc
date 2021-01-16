from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import hashlib
import itertools
from collections import deque

from lib.search import generic_bfs, StartEndPathProblem

MD5_DIRECTIONS = ["U", "D", "L", "R"]
DIRECTIONS = {
    "U": lambda x, y: (x, y - 1), 
    "D": lambda x, y: (x, y + 1),
    "L": lambda x, y: (x - 1, y),
    "R": lambda x, y: (x + 1, y),
}
md5 = lambda x: hashlib.md5((puzzle_input_raw + "".join(x)).encode("utf-8")).hexdigest()[:4]
is_open = lambda x: [int(c, base=16) > 10 for c in md5(x)]


def children(position, path):
    return [
        (p, path + tuple(d))
        for d, p
        in ((d, DIRECTIONS[d](*position)) for d in itertools.compress(MD5_DIRECTIONS, is_open(path)))
        if 0 <= p[0] <= 3 and 0 <= p[1] <= 3
    ]

problem = StartEndPathProblem((0, 0), (3, 3), children)
paths = generic_bfs(problem)

print(len(max(paths, key=len)))