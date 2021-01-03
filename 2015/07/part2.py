from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


wires = []
convert_parameter = lambda x: int(x) if x.isdigit() else x
for line in puzzle_input_raw.splitlines():
    wire_input, wire_name = line.split("->")
    wire_input_parameters = [x.strip() for x in wire_input.split()]

    if len(wire_input_parameters) == 1:
        wires.append((wire_name.strip(), "SET", convert_parameter(wire_input_parameters[0])))
    elif len(wire_input_parameters) == 2:
        wires.append((wire_name.strip(), "NOT", convert_parameter(wire_input_parameters[1])))
    else:
        wires.append((
            wire_name.strip(),
            wire_input_parameters[1], 
            convert_parameter(wire_input_parameters[0]), 
            convert_parameter(wire_input_parameters[2])
        ))

def resolve_all_wires(wires):
    resolved_wires = {}
    is_resolveable = lambda parameters: all(x in resolved_wires or isinstance(x, int) for x in parameters)
    get_parameter = lambda parameter: parameter if isinstance(parameter, int) else resolved_wires[parameter]
    while wires:
        wire, op, *parameters = wires.pop(0)
        if not is_resolveable(parameters):
            wires.append((wire, op, *parameters))
            continue

        if op == "SET":
            resolved_wires[wire] = get_parameter(parameters[0])
        elif op == "AND":
            resolved_wires[wire] = get_parameter(parameters[0]) & get_parameter(parameters[1])
        elif op == "OR":
            resolved_wires[wire] = get_parameter(parameters[0]) | get_parameter(parameters[1])
        elif op == "NOT":
            resolved_wires[wire] = get_parameter(parameters[0]) ^ 65535
        elif op == "LSHIFT":
            resolved_wires[wire] = get_parameter(parameters[0]) << get_parameter(parameters[1])
        elif op == "RSHIFT":
            resolved_wires[wire] = get_parameter(parameters[0]) >> get_parameter(parameters[1])
        else:
            wires.append((wire, op, *parameters))
    return resolved_wires


a = resolve_all_wires(wires.copy())["a"]
wires = [("b", "SET", a) if w == "b" else (w, o, *p) for w, o, *p in wires]
a = resolve_all_wires(wires.copy())["a"]
print(a)