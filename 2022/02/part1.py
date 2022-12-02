from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

SCORE_PLAYER_OFFSETS = [ord("A") - 1, ord("X") - 1]
PAYOFF = {
    (1, 1): 3,  # 0
    (1, 2): 0,  # -1
    (1, 3): 6,  # -2
    (2, 1): 6,  # 1
    (2, 2): 3,  # 0
    (2, 3): 0,  # -1
    (3, 1): 0,  # 2
    (3, 2): 6,  # 1
    (3, 3): 3,  # 0
}

plays = [x.split() for x in puzzle_input_raw.splitlines()]
scores = [ord(b) - SCORE_PLAYER_OFFSETS[1] + PAYOFF[(ord(b) - SCORE_PLAYER_OFFSETS[1], ord(a) - SCORE_PLAYER_OFFSETS[0])] for a, b in plays]
print(sum(scores))