import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# convert to list of inputs
puzzle_input = [int(x) for x in puzzle_input_raw.splitlines()]

# find puzzle input adding up to 2020
# for entry in puzzle_input:
    # for entry_2 in (x for x in puzzle_input if x < 2020 - entry):
        # matching_entry = 2020 - entry - entry_2
        # if matching_entry in puzzle_input:
            # print(f"Expense report sum: {entry * entry_2 * matching_entry}")
            # break
# else:
    # print("Unable to find matching entries in expense report")


permutations = {
    x + y + z: x * y * z
    for i, x in enumerate(puzzle_input)
    for j, y in enumerate(puzzle_input)
    for k, z in enumerate(puzzle_input)
    if i < j < k
}
answer = permutations[2020]

print(answer)
