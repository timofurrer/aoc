from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import functools

data = puzzle_input_raw
disk_size = 272

dragon_curve = lambda a: f"{a}0{''.join('1' if c == '0' else '0' for c in reversed(a))}"

while len(data) < disk_size:
    data = dragon_curve(data)

disk_data = data[:disk_size]
calc_checksum = lambda data: functools.reduce(lambda a, b: a + ("1" if b[0] == b[1] else "0"), zip(data[::2], data[1::2]), "")

checksum = calc_checksum(disk_data)
while len(checksum) % 2 == 0:
    checksum = calc_checksum(checksum)

print(checksum)