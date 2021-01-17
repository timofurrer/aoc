from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import string

polymer = puzzle_input_raw

def react(polymer):
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
    return len(polymer)

reactions = {}
for c in string.ascii_uppercase:
    prepped_polymer = polymer.replace(c, "").replace(c.lower(), "")
    reactions[c] = react(prepped_polymer)

print(min(reactions.items(), key=lambda x: x[1]))