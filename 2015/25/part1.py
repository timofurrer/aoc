from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re

code_coord = (int(re.findall("row (\d+)", puzzle_input_raw)[0]), int(re.findall(r"column (\d+)", puzzle_input_raw)[0]))

next_code = lambda c: (c * 252533) % 33554393
position_from_coord = lambda r, c: (r + c - 2) * (r + c - 1) // 2 + c - 1
first_code = 20151125
code_position = position_from_coord(*code_coord)

code = (first_code * pow(252533, code_position, 33554393)) % 33554393

# code = first_code
# for _ in range(code_position):
#     code = next_code(code)

print(code)