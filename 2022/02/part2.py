from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

OFFSET = ord("A") - 1
CHOICE = {
    ("A", "Y"): "A",
    ("A", "X"): "C",
    ("A", "Z"): "B",
    ("B", "Z"): "C",
    ("B", "Y"): "B",
    ("B", "X"): "A",
    ("C", "X"): "B",
    ("C", "Z"): "A",
    ("C", "Y"): "C",
}
PAYOFF = {"X": 0, "Y": 3, "Z": 6}

plays = [x.split() for x in puzzle_input_raw.splitlines()]

scores = [ord(CHOICE[a, b]) - OFFSET + PAYOFF[b] for a, b in plays]
print(sum(scores))