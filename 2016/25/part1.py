from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
import itertools

DIGIT = re.compile(r"-?\d+")

instructions = [x.split() for x in puzzle_input_raw.splitlines()]

def run(registers):
    pointer = 0
    clock_signal = []
    eval_param = lambda x: int(x) if DIGIT.fullmatch(x) else registers[x]
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
                pointer += eval_param(args[1])
            else:
                pointer += 1
        elif instr == "out":
            clock_signal.append(eval_param(args[0]))
            if len(clock_signal) >= 12:
                return all(x == 0 for x in clock_signal[::2]) and all(x == 1 for x in clock_signal[1::2])
            pointer += 1
        else:
            assert False, "unknown"

    return False


for i in itertools.count():
    if run({"a": i}):
        print("It's", i)
        break