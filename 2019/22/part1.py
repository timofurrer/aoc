from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with puzzle_input_path.open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

deck = list(range(0, 10_007))

for technique in puzzle_input_raw.splitlines():
    if technique == "deal into new stack":
        deck = list(reversed(deck))
    elif technique.startswith("cut"):
        cut = int(technique[4:])
        deck = deck[cut:] + deck[:cut]
    elif technique.startswith("deal with increment"):
        increment = int(technique[20:])
        new_deck = deck.copy()
        for idx, changed_idx in zip(range(len(deck)), (x % len(deck) for x in range(0, len(deck) * increment, increment))):
            new_deck[changed_idx] = deck[idx]
        deck = new_deck

print(deck.index(2019))