from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


def calc_req_total_fuel(mass):
    calc_req_fuel = lambda m: m // 3 - 2
    fuel = 0
    while mass > (2 * 3):
        mass = calc_req_fuel(mass)
        fuel += mass
    return fuel

total_required_fuel = sum(map(calc_req_total_fuel, (int(x) for x in puzzle_input_raw.splitlines())))
print(total_required_fuel)