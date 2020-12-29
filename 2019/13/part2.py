import sys
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
from queue import Queue

sys.path.insert(0, str(Path(__file__).parent.parent))
import intcode
sys.path = sys.path[1:]

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


program = [int(x) for x in puzzle_input_raw.split(",")]

with ThreadPoolExecutor(max_workers=1) as executor:

    scores = []
    paddle = None
    ball = None
    output_queue = Queue(maxsize=1)

    f = open("debug.txt", "w+")
    def inp():
        f.write(f"Input: {(ball > paddle) - (ball < paddle)}\n")
        f.flush()
        return (ball > paddle) - (ball < paddle)

    robot = executor.submit(
        intcode.run, 
        program, 
        # inputs=lambda: (ball > paddle) - (ball < paddle), 
        inputs=inp,
        outputs=output_queue, 
        initial_memory={0: 2}
    )

    while robot.running():
        x = output_queue.get()
        y = output_queue.get()
        tile_id_or_score = output_queue.get()
        f.write(f"Output: {x}, {y}, {tile_id_or_score}\n")
        f.flush()

        if not robot.done():
            if tile_id_or_score == 3:
                paddle = x
            elif tile_id_or_score == 4:
                ball = x
            if (x, y) == (-1, 0):
                scores.append(tile_id_or_score)
                print(scores)
    
    f.close()
    print(robot.result())

    executor.shutdown()

print(scores)