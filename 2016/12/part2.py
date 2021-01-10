from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import defaultdict

instructions = [x.split() for x in puzzle_input_raw.splitlines()]

registers = defaultdict(int, {"c": 1})
pointer = 0
eval_param = lambda x: int(x) if x.isdigit() else registers[x]
while pointer < len(instructions):
    instr, *args = instructions[pointer]
    if instr == "cpy":
        value = eval_param(args[0])
        registers[args[1]] = value
        pointer += 1
    elif instr == "inc":
        registers[args[0]] += 1
        pointer += 1
    elif instr == "dec":
        registers[args[0]] -= 1
        pointer += 1
    elif instr == "jnz":
        if eval_param(args[0]) != 0:
            pointer += int(args[1])
        else:
            pointer += 1
    else:
        assert False, "unknown"


print(registers["a"])