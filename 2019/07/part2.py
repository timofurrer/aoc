import sys
from pathlib import Path
import itertools
import concurrent.futures

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode, init_queue
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

THRUSTERS = 5
program = [int(x) for x in puzzle_input_raw.split(",")]
thruster_outputs = {}

for phase_settings in itertools.permutations(range(5, 10)):
    thruster_programs = [program.copy() for _ in range(THRUSTERS)]
    io_queues = [init_queue([p]) for p in phase_settings]
    io_queues[0].put(0)

    with concurrent.futures.ThreadPoolExecutor(max_workers=THRUSTERS) as executor:
        for i in range(THRUSTERS):
            intcode = Intcode(thruster_programs[i], inputs=io_queues[i], outputs=io_queues[(i + 1) if i < THRUSTERS - 1 else 0])
            executor.submit(intcode.run)
        
        executor.shutdown(wait=True)
        thruster_outputs[phase_settings] = io_queues[0].get()

print(max(x for x in thruster_outputs.values()))