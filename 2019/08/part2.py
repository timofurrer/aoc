from pathlib import Path
import itertools

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()
    
WIDTH = 25
HEIGHT = 6

layered_image = [puzzle_input_raw[i:i + WIDTH * HEIGHT] for i in range(0, len(puzzle_input_raw), WIDTH * HEIGHT)]
decoded_message = "".join([next(itertools.dropwhile(lambda x: x == "2", pixel_stack)) for pixel_stack in zip(*layered_image)])

for row in (decoded_message[i:i + WIDTH] for i in range(0, len(decoded_message), WIDTH)):
    for char in (row[i: i + 5] for i in range(0, len(row), 5)):
        print(char.replace("0", " ").replace("1", "0"), end="  ")
    print()