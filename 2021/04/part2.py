from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


raw_draws, *raw_boards = puzzle_input_raw.split("\n\n")
draws = raw_draws.split(",")
boards = [(False, [[(n, False) for n in x.split()] for x in board.split("\n")]) for board in raw_boards]


for draw in draws:
    for bi in range(len(boards)):
        if boards[bi][0]:
            continue

        board = boards[bi][1]
        for row in board:
            for i in range(len(row)):
                if row[i][0] == draw:
                    row[i] = (row[i][0], True)

        for b in (board, list(map(list, zip(*board)))):
            for row in b:
                if all(x[1] for x in row):
                    if len([b for b in boards if not b[0]]) == 1:
                        non_marked_sum = sum(int(y[0]) for x in board for y in x if not y[1])
                        print(non_marked_sum * int(draw))
                        exit()

                    boards[bi] = (True, board)
                    break
            if boards[bi][0]:
                break