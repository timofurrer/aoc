from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


from functools import cmp_to_key, reduce

packet_pairs = [[eval(x) for x in p.splitlines()] for p in puzzle_input_raw.replace("\n\n", "\n").splitlines()]

dividers = [[[2]], [[6]]]
packet_pairs.extend(dividers)


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


sorted_packets = sorted(packet_pairs, key = cmp_to_key(lambda l, r: -1 if is_pair_in_order(l, r) else 1))
print(reduce(lambda a, b: a * b, (sorted_packets.index(p) + 1 for p in dividers)))