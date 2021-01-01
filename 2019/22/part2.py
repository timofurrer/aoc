from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with puzzle_input_path.open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

deck_size = 119_315_717_514_047
shuffle_size = 101_741_582_076_661
card_to_track = 2020

single_increment = 1
single_offset = 0
for technique in puzzle_input_raw.splitlines():
    if technique == "deal into new stack":
        single_increment *= -1
        single_offset = (single_offset + single_increment) % deck_size
    elif technique.startswith("cut"):
        cut = int(technique[4:])
        single_offset = (single_offset + cut * single_increment) % deck_size
    elif technique.startswith("deal with increment "):
        incr = int(technique[20:])
        single_increment = (single_increment * pow(incr, deck_size - 2, deck_size)) % deck_size

increment = pow(single_increment, shuffle_size, deck_size)
offset = (single_offset * (1 - increment) * (pow(1 - single_increment, deck_size - 2, deck_size) % deck_size))
card_position = (offset + increment * card_to_track) % deck_size
print(card_position)