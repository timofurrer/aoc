from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from collections import Counter

ROOM_PATTERN = re.compile(r"([a-z-]+)-(\d+)\[([a-z]+)\]")
rooms = [ROOM_PATTERN.fullmatch(r).groups() for r in puzzle_input_raw.splitlines()]

checksum = lambda r: "".join(x for x, _ in sorted(Counter(r.replace("-", "")).most_common(), key=lambda x: (x[1], 26 - ord(x[0])), reverse=True)[:5])
is_real_room = lambda r, c: checksum(r) == c
decrypt = lambda r, s: "".join(" " if x == "-" else chr(ord("a") + (ord(x) - ord("a") + s) % 26) for x in r)

real_rooms = [r for r in rooms if is_real_room(r[0], r[2])]
northpole_object_storage = next(r for r in real_rooms if decrypt(r[0], int(r[1])).startswith("northpole"))
print(northpole_object_storage)