from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

print(sum(int(x) for x in puzzle_input_raw.splitlines()))