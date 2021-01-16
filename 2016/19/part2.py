from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

target = int(puzzle_input_raw)
i = 1

while i * 3 < target:
    i *= 3

print(target - i)