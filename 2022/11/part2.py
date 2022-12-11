from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import re
import math

monkeys = []
for i, monkey_raw in enumerate(l.splitlines() for l in puzzle_input_raw.split("\n\n")):
    levels = [int(x) for x in re.findall(r"\d+", monkey_raw[1])]
    op_e = re.search(r"= (.*?)$", monkey_raw[2]).group(1)
    op = lambda a, op_e=op_e: eval(op_e, {"old": a})
    divby = int(re.findall(r"\d+", monkey_raw[3])[0])
    t = int(re.findall(r"\d+", monkey_raw[4])[0])
    f = int(re.findall(r"\d+", monkey_raw[5])[0])
    throw_to = lambda x, t=t, f=f, divby=divby: t if x % divby == 0 else f
    monkeys.append({"levels": levels, "op": op, "throw_to": throw_to, "inspects": 0, "divby": divby})

modulus = math.lcm(*[m["divby"] for m in monkeys])
for round in range(10_000):
    for m, monkey in enumerate(monkeys):
        
        while monkey["levels"] and (item := monkey["levels"].pop(0)):
            monkey["inspects"] += 1
            item = monkey["op"](item) % modulus
            target_monkey = monkey["throw_to"](item)
            monkeys[target_monkey]["levels"].append(item)
    
x, y = sorted((m["inspects"] for m in monkeys), reverse=True)[:2]
print(x * y)