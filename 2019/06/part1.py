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

    def __repr__(self):
        return f"Node[{self.name}, children={self.children}, depth={self.depth()}]"


raw_orbits = dict([tuple(reversed(x.split(")"))) for x in puzzle_input_raw.splitlines()])
all_nodes = set()


def build_tree(start):
    start.children = [build_tree(Node(start, o)) for o, p in raw_orbits.items() if p == start.name]
    all_nodes.update(start.children)
    return start


result = build_tree(Node(None, "COM"))
print(sum(n.depth() for n in all_nodes))