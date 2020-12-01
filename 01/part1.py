import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# convert to list of inputs
puzzle_input = [int(x) for x in puzzle_input_raw.splitlines()]

# find puzzle input adding up to 2020
for entry in puzzle_input:
    matching_entry = 2020 - entry
    if matching_entry in puzzle_input:
        print(f"Expense report sum: {entry * matching_entry}")
        break
else:
    print("Unable to find matching entries in expense report")
