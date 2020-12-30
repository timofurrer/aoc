from collections import defaultdict, namedtuple
from queue import Queue

Parameter = namedtuple("Parameter", ["address", "value"])


def init_queue(initial_values):
    queue = Queue()
    for x in initial_values:
        queue.put(x)
    return queue


class Halt:
    PARAMETERS = 0
    def execute(self, intcode, parameters):
        intcode.halted = True
        return -1


class Add:
    PARAMETERS = 3
    def execute(self, intcode, parameters):
        intcode[parameters[2].address] = parameters[0].value + parameters[1].value 
        return intcode.advance_pointer(self.PARAMETERS)


class Mul:
    PARAMETERS = 3
    def execute(self, intcode, parameters):
        intcode[parameters[2].address] = parameters[0].value * parameters[1].value 
        return intcode.advance_pointer(self.PARAMETERS)


class Input:
    PARAMETERS = 1
    def execute(self, intcode, parameters):
        intcode[parameters[0].address] = intcode.input_provider()
        return intcode.advance_pointer(self.PARAMETERS)
        

class Output:
    PARAMETERS = 1
    def execute(self, intcode, parameters):
        intcode.output(parameters[0].value)
        return intcode.advance_pointer(self.PARAMETERS)
        

class JumpIfNotZero:
    PARAMETERS = 2
    def execute(self, intcode, parameters):
        if parameters[0].value != 0:
            return parameters[1].value
        else:
            return intcode.advance_pointer(self.PARAMETERS)
        

class JumpIfZero:
    PARAMETERS = 2
    def execute(self, intcode, parameters):
        if parameters[0].value == 0:
            return parameters[1].value
        else:
            return intcode.advance_pointer(self.PARAMETERS)
        

class LessThen:
    PARAMETERS = 3
    def execute(self, intcode, parameters):
        intcode[parameters[2].address] = int(parameters[0].value < parameters[1].value)
        return intcode.advance_pointer(self.PARAMETERS)
        

class Equal:
    PARAMETERS = 3
    def execute(self, intcode, parameters):
        intcode[parameters[2].address] = int(parameters[0].value == parameters[1].value)
        return intcode.advance_pointer(self.PARAMETERS)
        

class SetRelativeBaseOffset:
    PARAMETERS = 1
    def execute(self, intcode, parameters):
        intcode.relative_base += parameters[0].value
        return intcode.advance_pointer(self.PARAMETERS)


class Intcode:
    INSTRUCTIONS = {
        99: Halt,
        1: Add,
        2: Mul,
        3: Input,
        4: Output,
        5: JumpIfNotZero,
        6: JumpIfZero,
        7: LessThen,
        8: Equal,
        9: SetRelativeBaseOffset,
    }

    def __init__(self, code, inputs=None, outputs=None, initial_memory=None):
        self.code = code
        self.memory = defaultdict(int)
        self.inputs = inputs if inputs is not None else []
        self.outputs = outputs if outputs is not None else []
        self.pointer = 0
        self.relative_base = 0
        self.halted = False

        # initialize with given memory
        if initial_memory:
            for address, value in initial_memory.items():
                self[address] = value

        if callable(self.inputs):
            self.input_provider = inputs
        elif isinstance(self.inputs, Queue):
            self.input_provider = self.inputs.get
        elif isinstance(self.inputs, list):
            self.input_provider = lambda: self.inputs.pop(0)
        else:
            assert False, f"Unknown Input {type(self.inputs)}"

        if isinstance(self.outputs, Queue):
            self.output = self.outputs.put
            self.output_ready = lambda: not self.outputs.empty()
            self.output_consume = self.outputs.get
        elif isinstance(self.outputs, list):
            self.output = self.outputs.append
            self.output_ready = lambda: len(self.outputs) > 0
            self.output_consume = lambda: self.outputs.pop(0)
        else:
            assert False, f"Unknown Output {type(self.outputs)}"
        
    def run(self):
        while not self.halted:
            self.step()

        return self.outputs

    def step(self):
        self.pointer = self._execute_instruction_at(self.pointer)

    def step_to_next_output(self):
        while not self.halted and not self.output_ready():
            self.step()

        if self.halted:
            return None
        
        return self.output_consume()

    def _execute_instruction_at(self, pointer):
        instruction_code = str(self.code[pointer]).zfill(5)
        opcode = int(instruction_code[-2:])
        parameter_modes = instruction_code[:-2][::-1]
        instruction = self.INSTRUCTIONS[opcode]()

        # parse parameter w.r.t. the parameter mode
        parameters = []
        for i in range(instruction.PARAMETERS):
            value = self.code[pointer + 1 + i]  + (self.relative_base if parameter_modes[i] == "2" else 0)
            if parameter_modes[i] == "1":
                parameters.append(Parameter(value, value))
            else:
                parameters.append(Parameter(value, self[value]))

        return instruction.execute(self, parameters)

    def __getitem__(self, address):
        return self.code[address] if address < len(self.code) else self.memory[address]

    def __setitem__(self, address, value):
        if address < len(self.code):
            self.code[address] = value
        else:
            self.memory[address] = value

    def advance_pointer(self, parameters):
        return self.pointer + 1 + parameters