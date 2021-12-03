from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


diagnostic_codes = [x for x in puzzle_input_raw.split()]


def filter(diagnostic_codes, keep_func):
    i = 0
    while True:
        bits = list(zip(*diagnostic_codes))[i]
        keep_bit = keep_func(bits)
        diagnostic_codes = [c for c in diagnostic_codes if c[i] == keep_bit]
        if len(diagnostic_codes) == 1:
            return int(diagnostic_codes[0], base=2)

        i += 1


oxygen_generator_rating = filter(diagnostic_codes[:], lambda bits: str(int(sum(int(x) for x in bits) >= len(bits) / 2)))
co2_scrubber_rating = filter(diagnostic_codes[:], lambda bits: str(int(sum(int(x) for x in bits) < len(bits) / 2)))
life_support_rating = oxygen_generator_rating * co2_scrubber_rating
print(life_support_rating)