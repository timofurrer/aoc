import sys
from pathlib import Path
import itertools

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]

encode = lambda code: [ord(c) for c in code] + [ord("\n")]

springscript = [
    "NOT C J",
    "AND D J",
    "NOT A T",
    "OR T J",
    "WALK",
]

script_input = list(itertools.chain(*(encode(c) for c in springscript)))

springdroid = Intcode(program.copy(), inputs=script_input)
damage = springdroid.run()[-1]

print(damage)