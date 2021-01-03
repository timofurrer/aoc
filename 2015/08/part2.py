from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

total = 0
for code_string in puzzle_input_raw.splitlines():
    code_length = len(code_string)
    encoded_repr = code_string.replace("\\", "\\\\").replace('"', '\\"')
    encoded_length = len(encoded_repr) + 2
    total += encoded_length - code_length

print(total)