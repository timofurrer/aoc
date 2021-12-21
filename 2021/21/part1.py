from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


player_pos = [int(l[-1]) for l in puzzle_input_raw.splitlines()]


def roll_die(sides):
    n = 1
    while True:
        yield n
        n = (n + 1) % 100


die = roll_die(sides=100)

turn = 0
player_scores = [0, 0]
while True:
    t = turn % 2
    die_sum = sum(n for _, n in zip(range(3), die))
    player_pos[t] = (player_pos[t] + die_sum - 1) % 10 + 1
    player_scores[t] += player_pos[t]
    if player_scores[t] >= 1000:
        answer = (turn + 1) * 3 * player_scores[(turn + 1) % 2]
        print(answer)
        break
    turn += 1