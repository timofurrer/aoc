import os
import sys
import re
import functools

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

RULE_MATCHER = lambda x1, y1, x2, y2, x: x1 <= x <= y1 or x2 <= x <= y2
RULE_PATTERN = re.compile(r"(.+?): (\d+)-(\d+) or (\d+)-(\d+)")

rules_raw, your_ticket_raw, nearby_tickets_raw = puzzle_input_raw.split(os.linesep + os.linesep, maxsplit=3)

rules = {}
for rule_line in rules_raw.splitlines():
    name, x1, y1, x2, y2 = RULE_PATTERN.match(rule_line).groups()
    rules[name] = functools.partial(RULE_MATCHER, int(x1), int(y1), int(x2), int(y2))

your_ticket = [int(x) for x in your_ticket_raw.splitlines()[-1].split(",")]
nearby_tickets = [[int(x) for x in t.split(",")] for t in nearby_tickets_raw.splitlines()[1:]]

error_rate = sum(f for t in nearby_tickets for f in t if not any(r(f) for r in rules.values()))
print(error_rate)