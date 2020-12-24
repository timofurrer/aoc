import os
import sys
from collections import deque

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

players = [deque(int(x) for x in l[10:].split()) for l in puzzle_input_raw.split(os.linesep + os.linesep, maxsplit=2)]

while players[0] and players[1]:
    c1 = players[0].popleft()
    c2 = players[1].popleft()
    if c1 > c2:
        players[0].extend([c1, c2])
    else:
        players[1].extend([c2, c1])

winning_player_cards = next(p for p in players if p)
winning_score = sum(i * x for i, x in zip(range(len(winning_player_cards), 0, -1), winning_player_cards))
print(winning_score)