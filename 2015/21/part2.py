from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import itertools

#                 COST  DAMAGE    ARMOR
weapons = {
    "Dagger":       (8,      4,       0),
    "Shortsword":   (10,     5,       0),
    "Warhammer":    (25,     6,       0),
    "Longsword":    (40,     7,       0),
    "Greataxe":     (74,     8,       0),
}

armor = {
    "Leather":      (13,     0,       1),
    "Chainmail":    (31,     0,       2),
    "Splintmail":   (53,     0,       3),
    "Bandedmail":   (75,     0,       4),
    "Platemail":   (102,     0,       5),
    "no-armor":      (0,     0,       0),
}

rings = {
    "Damage +1":    (25,     1,       0),
    "Damage +2":    (50,     2,       0),
    "Damage +3":   (100,     3,       0),
    "Defense +1":   (20,     0,       1),
    "Defense +2":   (40,     0,       2),
    "Defense +3":   (80,     0,       3),
    "no-ring":       (0,     0,       0),
}
shop = {}
shop.update(weapons)
shop.update(armor)
shop.update(rings)

boss = tuple(int(x.split()[-1]) for x in puzzle_input_raw.splitlines())

losses = {}
for setting in ((w, a, r1, r2) for w, a, r1, r2 in itertools.product(weapons, armor, rings, rings) if r1 != r2 or r1 == "no-ring"):
    setting_total = tuple(map(sum, zip(*(shop[x] for x in setting))))
    player_dealt_damage = max(1, setting_total[1] - boss[2])
    boss_dealt_damage = max(1, boss[1] - setting_total[2])
    if boss[0] / player_dealt_damage >= 100 / boss_dealt_damage:
        losses[setting] = setting_total[0]

minimal_cost_win = max(losses.items(), key=lambda c: c[1])
print(minimal_cost_win)