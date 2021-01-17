from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

polymer = puzzle_input_raw

while True:
    idx = 0
    new_polymer = []
    while idx < len(polymer):
        if idx + 1 >= len(polymer):
            new_polymer.append(polymer[idx])
            idx += 1
            continue

        a, b = polymer[idx:idx + 2]
        if a != b and a.lower() == b.lower():
            idx += 2
        else:
            new_polymer.append(a)
            idx += 1

    if polymer == new_polymer:
        break

    polymer = new_polymer

print(len(polymer))