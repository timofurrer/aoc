from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

contains_at_least_three_vowels = lambda w: len([x for x in w if x in {"a", "e", "i", "o", "u"}]) >= 3
two_letters_in_row = lambda w: any(x == y for x, y in zip(w, w[1:]))
does_not_contain = lambda w: all(x not in w for x in {"ab", "cd", "pq", "xy"})

is_nice = lambda w: contains_at_least_three_vowels(w) and two_letters_in_row(w) and does_not_contain(w)

nice = sum(is_nice(w) for w in puzzle_input_raw.splitlines())
print(nice)