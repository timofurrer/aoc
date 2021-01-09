from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
import string
import itertools

compressed = puzzle_input_raw

MARKER_PATTERN = re.compile(r"\((\d+)x(\d+)\)")

def decompress(compressed):
    decompressed_length = 0
    idx = 0
    while idx < len(compressed):
        if compressed[idx] != "(":
            decompressed_length += 1
            idx += 1
        else:
            match = MARKER_PATTERN.match(compressed[idx:])
            x, y = tuple(int(x) for x in match.groups())
            start = idx + match.end()
            decompressed_length += decompress(compressed[start:start + x]) * y
            idx += x + match.end()
    return decompressed_length

print(decompress(compressed))