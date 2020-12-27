import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
import intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]
print(intcode.run(program, inputs=[5]))