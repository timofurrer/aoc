from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re

DIGIT = re.compile(r"-?\d+")

instructions = [x.split() for x in puzzle_input_raw.splitlines()]

registers = {"a": 7}
pointer = 0
eval_param = lambda x: int(x) if DIGIT.fullmatch(x) else registers[x]
while pointer < len(instructions):
    instr, *args = instructions[pointer]
    try:
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
                pointer += eval_param(args[1])
            else:
                pointer += 1
        elif instr == "tgl":
            p = pointer + eval_param(args[0])
            pointer += 1
            if not (0 <= p < len(instructions)):
                continue

            name, *arguments = instructions[p]
            if len(arguments) == 1:
                if name == "inc":
                    instructions[p] = ["dec"] + arguments
                else:
                    instructions[p] = ["inc"] + arguments
            elif len(arguments) == 2:
                if name == "jnz":
                    instructions[p] = ["cpy"] + arguments
                else:
                    instructions[p] = ["jnz"] + arguments
        else:
            assert False, "unknown"
    except KeyError as exc:
        pointer += 1


print(registers["a"])