from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

blocked_ip_ranges = sorted(tuple(int(x) for x in x.split("-")) for x in puzzle_input_raw.splitlines())

allowed_ips = 0
for start, end in blocked_ip_ranges:
    next_lowest_ip = end + 1
    if not any(x[0] <= next_lowest_ip <= x[1] for x in blocked_ip_ranges):
        higher_ips = [x for x in blocked_ip_ranges if x[0] > next_lowest_ip]
        if not higher_ips:
            break

        closest = min(higher_ips, key=lambda x: x[0] - next_lowest_ip)[0]
        allowed_ips += closest - next_lowest_ip
        
print(allowed_ips)