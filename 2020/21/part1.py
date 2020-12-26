import os
import sys
import re

puzzle_input_path = os.path.join(os.path.dirname(__file__), "input_1.txt")

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


foods = []
all_allergens = set()
for line in puzzle_input_raw.splitlines():
    match = re.fullmatch(r"(.*?) \(contains (.*?)\)", line)
    ingredients = set(match.group(1).split())
    allergens = {x.strip() for x in match.group(2).split(",")}
    all_allergens.update(allergens)
    foods.append((ingredients, allergens))


ingredient_allergen_map = {}
while all_allergens:
    for allergen in all_allergens:
        # get all foods containing this allergene
        foods_with_allergen = [i for i, a in foods if allergen in a]
        ingredient_intersection = set.intersection(*foods_with_allergen)
        ingredient_intersection.difference_update(ingredient_allergen_map.keys())
        if len(ingredient_intersection) == 1:
            ingredient_allergen_map[list(ingredient_intersection)[0]] = allergen
            all_allergens.remove(allergen)
            break

# ingredients with no allergenes
ingredients_wo_allergen = [i for ingredients, _ in foods for i in ingredients if i not in ingredient_allergen_map]
print(len(ingredients_wo_allergen))