from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import Counter

columns = list(map(tuple, zip(*puzzle_input_raw.splitlines())))

message = [Counter(c).most_common(1)[0][0] for c in columns]
print("".join(message))