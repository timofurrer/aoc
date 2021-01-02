import sys
from pathlib import Path
from collections import deque
import itertools
from concurrent.futures import ThreadPoolExecutor

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]

input_queue = deque()
output_queue = deque()
robot = Intcode(program, inputs=input_queue, outputs=output_queue)

def run_until_command():
    output = []
    while "Command?" not in "".join(chr(x) if x else "" for x in output) and not robot.halted:
        output_char = robot.step_to_next_output()
        output.append(output_char)

    return "".join(chr(x) if x else "" for x in output)

def send_command(cmd): 
    input_queue.extend([ord(x) for x in cmd] + [ord("\n")])
    return run_until_command()

run_until_command()
with (Path(__file__).parent / "collect.txt").open() as collect_file:
    for command in (c.strip() for c in collect_file):
        output = send_command(command)
        print(output)
    
items = set(["pointer", "hypercube", "cake", "tambourine", "monolith", "mouse", "coin", "mug"])
combinations = itertools.chain(*(itertools.combinations(items, r + 1) for r in range(len(items))))
for combination in combinations:
    for item in items:
        if item in combination:
            send_command(f"take {item}")
        else:
            send_command(f"drop {item}")
    output = send_command("north")
    print(output)
    if "Analysis complete" in output:
        print(f"correct combination: {combination}")
        break