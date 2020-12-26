from pathlib import Path
from collections import Counter

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


def possible_passwords(start, end):
    password_range = range(start, end + 1)
    # digits are always increasing in a number if after sorting the digits the number is still the same
    has_only_increasing_digits = lambda password: password == "".join(sorted(password))
    # check if at least two digits are the same
    at_least_two_digits_same = lambda password: any(x >= 2 for x in Counter(password).values())
    
    possible_passwords = [
        p 
        for p 
        in password_range 
        if has_only_increasing_digits(str(p)) and at_least_two_digits_same(str(p))
    ]
    
    return possible_passwords

start, end = [int(x) for x in puzzle_input_raw.split("-")]
print(len(possible_passwords(start, end)))