import os
import sys
import regex as re

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

rules_raw, received_messages_raw = puzzle_input_raw.split(os.linesep + os.linesep, maxsplit=2)
rules = {n: [x.replace('"', "").strip() for x in r.split("|")] for n, r in (x.split(":") for x in rules_raw.splitlines())}

# rules adaptions, using 3rd-party 'regex' package for recursive groups ;)
rules["8"] = ["42 +"]
rules["11"] = ["(?P<R> 42 (?&R)? 31 )"]


def transpile_to_regex(start, rules):
    output_terms = []
    for or_term in start:
        output = []
        for term in or_term.split():
            if not term.isdigit():
                output.append(term)
            else:
                output.append(transpile_to_regex(rules[term], rules))
        output_terms.append("".join(output))
    return "(?:" + "|".join(output_terms) + ")"


regex = re.compile(transpile_to_regex(rules['0'], rules))
matches = sum(1 for m in received_messages_raw.splitlines() if regex.fullmatch(m))
print(matches)