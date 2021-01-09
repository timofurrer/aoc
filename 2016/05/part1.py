from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import hashlib
import itertools

door_id = puzzle_input_raw

md5 = lambda x: hashlib.md5(x.encode("utf-8")).hexdigest()
all_zeros = lambda x: all(x == "0" for x in x)
password_generator = (md5(x)[5] for x in (door_id + str(i) for i in itertools.count()) if all_zeros(md5(x)[:5]))

password = [next(password_generator) for _ in range(8)]
print("".join(password))