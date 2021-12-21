from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


from functools import lru_cache
from collections import Counter
from itertools import product

player_pos = [int(l[-1]) for l in puzzle_input_raw.splitlines()]
die_sums_for_universes = Counter(sum(t) for t in product([1, 2, 3], repeat=3))


@lru_cache(maxsize=None)
def game(player_pos_start, player_scores_start, turn):
    wins = [0, 0]

    for die_sum, universes in die_sums_for_universes.items():
        player_pos = list(player_pos_start)
        player_scores = list(player_scores_start)

        player_pos[turn] = (player_pos[turn] + die_sum - 1) % 10 + 1
        player_scores[turn] += player_pos[turn]

        if player_scores[turn] >= 21:
            wins[turn] += universes
        else:
            for i, subwin in enumerate(game(tuple(player_pos), tuple(player_scores), (turn + 1) % 2)):
                wins[i] += subwin * universes
    return wins


wins = game(tuple(player_pos), (0, 0), 0)
print(max(wins))