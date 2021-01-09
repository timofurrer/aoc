from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import hashlib
import itertools

door_id = puzzle_input_raw

md5 = lambda x: hashlib.md5(x.encode("utf-8")).hexdigest()
all_zeros = lambda x: all(x == "0" for x in x)
password_generator = (x for x in (md5(door_id + str(i)) for i in itertools.count()) if all_zeros(x[:5]))

password = [None for _ in range(8)]
while any(x is None for x in password):
    p = next(password_generator)
    pos = int(p[5], base=16)
    if 0 <= pos <= 7 and password[pos] is None:
        password[pos] = p[6]

print("".join(password))