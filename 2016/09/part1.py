from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
import itertools

compressed = puzzle_input_raw

MARKER_PATTERN = re.compile(r"\((\d+)x(\d+)\)")

idx = 0
decompressed = ""
while idx < len(compressed):
    if compressed[idx] != "(":
        chars = "".join(itertools.takewhile(lambda x: x != "(", compressed[idx:]))
        decompressed += chars
        idx += len(chars)
    else:
        match = MARKER_PATTERN.match(compressed[idx:])
        x, y = tuple(int(x) for x in match.groups())
        start = idx + match.end()
        decompressed += compressed[start:start + x] * y
        idx = start + x

print(len(decompressed))