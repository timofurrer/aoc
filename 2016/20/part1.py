from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

blocked_ip_ranges = sorted(tuple(int(x) for x in x.split("-")) for x in puzzle_input_raw.splitlines())

for start, end in blocked_ip_ranges:
    next_lowest_ip = end + 1
    if not any(x[0] <= next_lowest_ip <= x[1] for x in blocked_ip_ranges):
        print(f"Found {next_lowest_ip}")
        break