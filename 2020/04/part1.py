import os
import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


required_keys = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

# parse puzzle input
passports = puzzle_input_raw.split(os.linesep + os.linesep)

valid_passports = [
    p
    for p
    in passports
    if {x.split(":")[0] for x in p.split()}.issuperset(required_keys)
]
print(len(valid_passports))
