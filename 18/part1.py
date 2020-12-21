import os
import sys
import re
from collections import deque

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


# Simple Shunting-Yard Implementation: 
# see https://en.wikipedia.org/wiki/Shunting-yard_algorithm#The_algorithm_in_detail

OPERATORS = {"+": 0, "*": 0}
def create_and_push(token, output):
    if token.isdigit():
        output.append(int(token))
    elif token == "+":
        output.append(output.pop() + output.pop())
    elif token == "*":
        output.append(output.pop() * output.pop())


results = []
for infix in puzzle_input_raw.splitlines():
    tokens = tokens = infix.replace('(', ' ( ').replace(')', ' ) ').strip().split()
    output = deque()
    operators = deque()
    for token in tokens:
        if token.isdigit():
            create_and_push(token, output)
        elif token in OPERATORS:
            while (
                (len(operators) >= 1) and
                (operators[-1] in OPERATORS) and
                (OPERATORS[operators[-1]] >= OPERATORS[token])
            ):
                create_and_push(operators.pop(), output)
            operators.append(token)
        elif token == "(":
            operators.append(token)
        elif token == ")":
            while len(operators) >= 1 and operators[-1] != "(":
                create_and_push(operators.pop(), output)
            if len(operators) >= 1 and operators[-1] == "(":
                operators.pop()

    while operators:
        create_and_push(operators.pop(), output)

    results.append(output.pop())


result_sum = sum(results)
print(result_sum)