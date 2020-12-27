import sys
from pathlib import Path
import itertools

sys.path.insert(0, str(Path(__file__).parent.parent))
import intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

THRUSTERS = 5
program = [int(x) for x in puzzle_input_raw.split(",")]
thrusters = [program.copy() for _ in range(THRUSTERS)]
thruster_outputs = {}

for phase_settings in itertools.permutations(range(THRUSTERS)):
    phase_provider = iter(phase_settings)
    input_signal = 0
    for thruster in thrusters:
        output_signal = intcode.run(thruster, inputs=[next(phase_provider), input_signal])
        input_signal = output_signal[0]
    thruster_outputs[phase_settings] = input_signal

print(max(x for x in thruster_outputs.values()))