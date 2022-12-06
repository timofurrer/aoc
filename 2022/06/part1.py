from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


START_OF_PAKET_SIZE = 4

start = next(i + START_OF_PAKET_SIZE for i in range(len(puzzle_input_raw)) if len(set(puzzle_input_raw[i:i + START_OF_PAKET_SIZE])) == START_OF_PAKET_SIZE)
print(start)