import sys
from pathlib import Path
from queue import Queue
import itertools
from concurrent.futures import ThreadPoolExecutor

sys.path.insert(0, str(Path(__file__).parent.parent))
from intcode import Intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

program = [int(x) for x in puzzle_input_raw.split(",")]


def computer_input(address, queue):
    # send address to computer for init
    yield address
    # send receiving packets into the computers input queue
    while True:
        try:
            x = queue.get(timeout=0.2)
        except Exception:
            yield - 1
        else:
            yield x  # X value
            yield queue.get()  # Y value

input_queues = [Queue() for _ in range(50)]
output_queues = [Queue() for _ in range(50)]
computers = [Intcode(program.copy(), inputs=computer_input(i, input_queues[i]), outputs=output_queues[i]) for i in range(50)]

with ThreadPoolExecutor(max_workers=50) as executor:

    for computer in computers:
        executor.submit(computer.run)

    for sender, output_queue in itertools.cycle(enumerate(output_queues)):
        try:
            receiver_address = output_queue.get_nowait()
        except Exception:
            continue

        x = output_queue.get()
        y = output_queue.get()

        if receiver_address == 255:
            print(y)
            break

        input_queues[receiver_address].put(x)
        input_queues[receiver_address].put(y)

    for c in computers:
        c.shutdown()
        
    executor.shutdown()