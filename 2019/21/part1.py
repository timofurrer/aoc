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
    "NOT A T",
    "NOT B J",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "NOT E T",
    "NOT T T",
    "OR H T",
    "AND T J",
    "RUN",
]

script_input = list(itertools.chain(*(encode(c) for c in springscript)))

springdroid = Intcode(program.copy(), inputs=script_input)
damage = springdroid.run()[-1]

print(damage)