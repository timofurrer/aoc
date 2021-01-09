from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from collections import defaultdict

program = [tuple(x.strip(",") for x in x.split()) for x in puzzle_input_raw.splitlines()]

registers = defaultdict(int, {"a": 1})
pointer = 0

while pointer < len(program):
    instr, *args = program[pointer]
    if instr == "hlf":
        registers[args[0]] //= 2
        pointer += 1
    elif instr == "tpl":
        registers[args[0]] *= 3
        pointer += 1
    elif instr == "inc":
        registers[args[0]] += 1
        pointer += 1
    elif instr == "jmp":
        pointer += int(args[0])
    elif instr == "jie":
        if registers[args[0]] % 2 == 0:
            pointer += int(args[1])
        else:
            pointer += 1
    elif instr == "jio":
        if registers[args[0]] == 1:
            pointer += int(args[1])
        else:
            pointer += 1
    else:
        assert False, "unknown instruction"

print(registers)