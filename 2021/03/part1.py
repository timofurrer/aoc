from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


diagnostic_codes = [x for x in puzzle_input_raw.split()]

gamma_rate = int(
    "".join(
        str(b) for b in
        (int(sum(int(x) for x in xs) > len(xs) / 2) for xs in zip(*diagnostic_codes))
    ),
    base=2
)
epsilon_rate = (~gamma_rate & 0xFFF)

power_consumption = gamma_rate * epsilon_rate
print(power_consumption)