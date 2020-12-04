import os
import re
import sys

puzzle_input_path = sys.argv[1]

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


required_keys = {
    "byr": lambda x: 1920 <= int(x) <= 2002,
    "iyr": lambda x: 2010 <= int(x) <= 2020,
    "eyr": lambda x: 2020 <= int(x) <= 2030,
    "hgt": lambda x: 150 <= int(x[:-2]) <= 193 if x.endswith("cm") else (59 <= int(x[:-2]) <= 76 if x.endswith("in") else False),
    "hcl": lambda x: re.match(r"^#[0-9a-f]{6}$", x) is not None,
    "ecl": lambda x: x in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"},
    "pid": lambda x: re.match(r"^\d{9}$", x) is not None
}

# parse puzzle input
passports = puzzle_input_raw.split(os.linesep + os.linesep)

count = 0
for passport in passports:
    fields = [x.split(":") for x in passport.split()]
    if all(required_keys[key](value) for key, value in fields if key in required_keys) and {x[0] for x in fields}.issuperset(required_keys):
        count += 1

print(count)
