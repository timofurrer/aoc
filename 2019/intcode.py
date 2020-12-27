import operator


def run(program, inputs):
    pointer = 0
    input_provider = iter(inputs)
    outputs = []

    OPERATORS = {1: operator.add, 2: operator.mul}

    while program[pointer] != 99:
        instruction = str(program[pointer]).zfill(5)
        opcode = int(instruction[-2:])
        get_parameter = lambda i: program[program[pointer + i]] if list(reversed(instruction[:-2]))[i - 1] == "0" else program[pointer + i]
        if opcode in OPERATORS:
            op = OPERATORS[opcode]
            program[program[pointer + 3]] = op(get_parameter(1), get_parameter(2))
            pointer += 4
        elif opcode == 3:
            data = int(next(input_provider))
            program[program[pointer + 1]] = data
            pointer += 2
        elif opcode == 4:
            data = get_parameter(1)
            outputs.append(data)
            pointer += 2
        elif opcode == 5:
            if get_parameter(1) != 0:
                pointer = get_parameter(2)
            else:
                pointer += 3
        elif opcode == 6:
            if get_parameter(1) == 0:
                pointer = get_parameter(2)
            else:
                pointer += 3
        elif opcode == 7:
            program[program[pointer + 3]] = int(get_parameter(1) < get_parameter(2))
            pointer += 4
        elif opcode == 8:
            program[program[pointer + 3]] = int(get_parameter(1) == get_parameter(2))
            pointer += 4
    
    return outputs