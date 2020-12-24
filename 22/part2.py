import os
import sys
from collections import deque

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

players = [deque(int(x) for x in l[10:].split()) for l in puzzle_input_raw.split(os.linesep + os.linesep, maxsplit=2)]


def combat(player1_cards, player2_cards):
    played = set()
    while player1_cards and player2_cards:
        play = (tuple(player1_cards), tuple(player2_cards))
        if play in played:
            return True
        played.add(play)

        c1 = player1_cards.popleft()
        c2 = player2_cards.popleft()
        if c1 <= len(player1_cards) and c2 <= len(player2_cards):
            player1_wins_round = combat(
                deque(list(player1_cards)[:c1]), deque(list(player2_cards)[:c2])
            )
        else:
            player1_wins_round = c1 > c2

        if player1_wins_round:
            player1_cards.extend([c1, c2])
        else:
            player2_cards.extend([c2, c1])

    return bool(player1_cards)


combat(players[0], players[1])
winning_player_cards = next(p for p in players if p)
winning_score = sum(i * x for i, x in zip(range(len(winning_player_cards), 0, -1), winning_player_cards))
print(winning_score)