from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with puzzle_input_path.open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()  

print(puzzle_input_raw)