from pathlib import Path

puzzle_input_path = Path(__file__).parent / "input.txt"

with open(puzzle_input_path) as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


class Node:
    def __init__(self, parent, name):
        self.name = name
        self.children = []
        self.parent = parent

    def depth(self):
        if not self.parent:
            return 0

        return self.parent.depth() + 1

    def path(self):
        if not self.parent:
            return [self.name]
        
        return self.parent.path() + [self.name]

    def __repr__(self):
        return f"Node[{self.name}, path={self.path()}, depth={self.depth()}]"


raw_orbits = dict([tuple(reversed(x.split(")"))) for x in puzzle_input_raw.splitlines()])
com = Node(None, "COM")
all_nodes = {}

def build_tree(start):
    start.children = [build_tree(Node(start, o)) for o, p in raw_orbits.items() if p == start.name]
    all_nodes.update({c.name: c for c in start.children})
    return start


all_nodes["COM"] = build_tree(com)
you = all_nodes["YOU"]
santa = all_nodes["SAN"]
intersections = (all_nodes[i] for i in set(you.path()).intersection(set(santa.path())))
you_to_santa = min(you.depth() - i.depth() + santa.depth() - i.depth() - 2 for i in intersections)
print(you_to_santa)