import os
import sys
import math
import itertools

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


seats = [
    int(s.replace("F", "0").replace("L", "0").replace("B", "1").replace("R", "1"), base=2)
    for s
    in puzzle_input_raw.splitlines()
]

print(max(seats))


#MAX_ROW = 127
#MAX_COL = 7

#MOVES = {
#    "F": lambda lower, upper: (lower, math.floor((lower + upper) / 2)),
#    "B": lambda lower, upper: (math.ceil((lower + upper) / 2), upper),
#    "L": lambda lower, upper: (lower, math.floor((lower + upper) / 2)),
#    "R": lambda lower, upper: (math.ceil((lower + upper) / 2), upper),
#}

#seats = []
#for seat in puzzle_input_raw.splitlines():
#    lower, upper = 0, MAX_ROW
#    for r in seat[:7]:
#        lower, upper = MOVES[r](lower, upper)
#    
#    row = lower

#    lower, upper = 0, MAX_COL
#    for c in seat[7:]:
#        lower, upper = MOVES[c](lower, upper)
    
#    col = upper

#    final_seat = row * 8 + col
#    seats.append(final_seat)

#print(max(seats))