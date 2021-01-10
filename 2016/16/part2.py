from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

data = puzzle_input_raw
disk_size = 35651584

dragon_curve = lambda a: f"{a}0{''.join('1' if c == '0' else '0' for c in reversed(a))}"

while len(data) < disk_size:
    data = dragon_curve(data)

disk_data = data[:disk_size]

def calc_checksum(data):
    checksum = ("1" if a == b else "0" for a, b in zip(data[::2], data[1::2]))
    return "".join(checksum)

checksum = calc_checksum(disk_data)
while len(checksum) % 2 == 0:
    checksum = calc_checksum(checksum)

print(checksum)