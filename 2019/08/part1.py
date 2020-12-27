from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()
    
WIDTH = 25
HEIGHT = 6

layered_image = [puzzle_input_raw[i:i + WIDTH * HEIGHT] for i in range(0, len(puzzle_input_raw), WIDTH * HEIGHT)]
result = min(layered_image, key=lambda l: l.count("0"))
print(result.count("1") * result.count("2"))