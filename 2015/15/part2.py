from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import re
import itertools
import math

INGREDIENT_PATTERN = re.compile(r"capacity (?P<capacity>-?\d+), durability (?P<durability>-?\d+), flavor (?P<flavor>-?\d+), texture (?P<texture>-?\d+), calories (?P<calories>-?\d+)")
MAX_INGREDIENTS = 100

ingredients = {x[0]: [int(v) for v in INGREDIENT_PATTERN.search(x[1]).groups()] for x in (x.split(maxsplit=1) for x in puzzle_input_raw.splitlines())}
scores = {}
for ingredients_amounts in (x for x in itertools.product(*(range(1, MAX_INGREDIENTS - len(ingredients) + 2) for _ in range(len(ingredients)))) if sum(x) == MAX_INGREDIENTS):
    ingredient_values = list(zip(*ingredients.values()))
    if sum(map(math.prod, zip(ingredients_amounts, ingredient_values[-1]))) != 500:
        continue 

    score = math.prod(max(0, sum(map(math.prod, zip(ingredients_amounts, x)))) for x in ingredient_values[:-1])
    scores[ingredients_amounts] = score

best_recipe = max(scores.items(), key=lambda x: x[1])
print(best_recipe)