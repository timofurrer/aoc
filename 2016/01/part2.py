from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

path = [(x[0], int(x[1:])) for x in puzzle_input_raw.split(", ")]

TURN = {
    "L": lambda d: (-d[1], d[0]),
    "R": lambda d: (d[1], -d[0]),
}


def walk(path):
    current_direction = (1, 0)
    current_position = (0, 0)
    visited = {current_position}
    for turn, forward in path:
        current_direction = TURN[turn](current_direction)

        for _ in range(forward):
            current_position = tuple(map(sum, zip(current_position, current_direction)))
            if current_position in visited:
                return current_position

            visited.add(current_position)

    

end_position = walk(path)
print(sum(abs(x) for x in end_position))