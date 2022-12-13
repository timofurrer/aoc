from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


packet_pairs = [[eval(x) for x in p.splitlines()] for p in puzzle_input_raw.split("\n\n")]


def is_pair_in_order(l, r):
    end = max(len(l), len(r))
    for i in range(end):
        if i == len(l) and i < len(r):
            return True
        if i < len(l) and i == len(r):
            return False      

        match l[i], r[i]:
            case int(lv), int(rv):
                if lv < rv:
                    return True
                if lv > rv:
                    return False
            case list(lv), list(rv):
                if (o := is_pair_in_order(lv, rv)) is not None:
                    return o
            case int(lv), list(rv):
                if (o := is_pair_in_order([lv], rv)) is not None:
                    return o
            case list(lv), int(rv):
                if (o := is_pair_in_order(lv, [rv])) is not None:
                    return o
    
    return None

print(sum(i for i, (l, r) in enumerate(packet_pairs, start=1) if is_pair_in_order(l, r)))