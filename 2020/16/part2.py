import os
import sys
import re
import functools

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

RULE_MATCHER = lambda x1, y1, x2, y2, t: all(x1 <= x <= y1 or x2 <= x <= y2 for x in t)
RULE_PATTERN = re.compile(r"(.+?): (\d+)-(\d+) or (\d+)-(\d+)")

rules_raw, your_ticket_raw, nearby_tickets_raw = puzzle_input_raw.split(os.linesep + os.linesep, maxsplit=3)

rules = {}
for rule_line in rules_raw.splitlines():
    name, x1, y1, x2, y2 = RULE_PATTERN.match(rule_line).groups()
    rules[name] = functools.partial(RULE_MATCHER, int(x1), int(y1), int(x2), int(y2))

your_ticket = [int(x) for x in your_ticket_raw.splitlines()[-1].split(",")]
nearby_tickets = [[int(x) for x in t.split(",")] for t in nearby_tickets_raw.splitlines()[1:]]
valid_tickets = [t for t in nearby_tickets if any(r(t) for r in rules.values())]

valid_tickets_T = list(zip(range(len(valid_tickets)), *valid_tickets))
available_rules = rules.copy()
departures = 1
while available_rules and valid_tickets_T:
    for idx, *ticket_fields in valid_tickets_T:
        rule_candidates = [(n, r) for n, r in available_rules.items() if r(ticket_fields)]
        if len(rule_candidates) == 1:
            del available_rules[rule_candidates[0][0]]
            valid_tickets_T.remove((idx, *ticket_fields))

            if rule_candidates[0][0].startswith("departure"):
                departures *= your_ticket[idx]

            break

print(departures)