from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
import math
from collections import defaultdict

instructions = puzzle_input_raw.splitlines()

GIVE_VALUE_PATTERN = re.compile(r"value (\d+) goes to bot (\d+)")
BOT_GIVE_PATTERN = re.compile(r"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")

bots = defaultdict(set)
outputs = defaultdict(set)
targets = {"bot": bots, "output": outputs}

while instructions:
    instruction = instructions.pop(0)
    if instruction.startswith("value"):
        v, b = tuple(int(x) for x in GIVE_VALUE_PATTERN.match(instruction).groups())
        bots[b].add(int(v))
    else:
        b, low_type, low_target, high_type, high_target = tuple(int(x) if x.isdigit() else x for x in BOT_GIVE_PATTERN.match(instruction).groups())
        if len(bots[b]) != 2:
            instructions.append(instruction)
        else:
            low, high = sorted(bots[b])
            targets[low_type][low_target].add(low)
            targets[high_type][high_target].add(high)
            bots[b].clear()


print(math.prod(list(v)[0] for o, v in outputs.items() if o in {0, 1, 2}))