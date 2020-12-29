import time
import operator
from collections import deque, defaultdict
from queue import Queue


def run(program, inputs, outputs=None, initial_memory=None):
    pointer = 0

    outputs = [] if outputs is None else outputs
    memory = defaultdict(int)
    relative_base = 0

    if callable(inputs):
        inputs_provider = inputs
    else:
        inputs = deque(inputs) if not isinstance(inputs, deque) else inputs
        def inputs_provider():
            while True:
                try:
                    return int(inputs.popleft())
                except IndexError:
                    continue

    if isinstance(outputs, Queue):
        def output(x):
            outputs.put(x)
            while not outputs.empty():
                time.sleep(0.01)  # wait until consumed
    else:
        output = lambda x: outputs.append(x)


    def get(address):
        return program[address] if address < len(program) else memory[address]

    def set(address, value):
        if address < len(program):
            program[address] = value
        else:
            memory[address] = value

    # set the initial memory
    if initial_memory:
        for address, value in initial_memory.items():
            set(address, value)

    OPERATORS = {1: operator.add, 2: operator.mul}

    while program[pointer] != 99:
        instruction = str(program[pointer]).zfill(5)
        opcode = int(instruction[-2:])
        modes = instruction[:-2][::-1]

        def get_parameter(i):
            mode = modes[i - 1]
            if mode == "0":
                return get(program[pointer + i])
            elif mode == "1":
                return get(pointer + i)
            elif mode == "2":
                return get(relative_base + program[pointer + i])
            else:
                assert False, "unknown mode"

        def set_result(i, value):
            mode = modes[i - 1]
            if mode == "0":
                return set(program[pointer + i], value)
            elif mode == "2":
                return set(relative_base + program[pointer + i], value)
            else:
                assert False, "unknown mode"


        if opcode in OPERATORS:
            op = OPERATORS[opcode]
            set_result(3, op(get_parameter(1), get_parameter(2)))
            pointer += 4
        elif opcode == 3:
            data = inputs_provider()
            set_result(1, data)
            pointer += 2
        elif opcode == 4:
            data = get_parameter(1)
            output(data)
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
            set_result(3, int(get_parameter(1) < get_parameter(2)))
            pointer += 4
        elif opcode == 8:
            set_result(3, int(get_parameter(1) == get_parameter(2)))
            pointer += 4
        elif opcode == 9:
            relative_base += get_parameter(1)
            pointer += 2
            
    return outputs