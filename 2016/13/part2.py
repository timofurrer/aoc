from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from lib.search import GridGraph, StartEndGraphProblem, explore_nodes_in_step_limit

is_wall = lambda x, y: bin((x * x + 3 * x + 2 * x * y + y + y * y) + int(puzzle_input_raw)).count("1") % 2 != 0


class DynamicGridGraph(GridGraph):
    def children(self, loc):
        # only return children which are not a well
        return [l for l in self.generate_4_neighbors(loc) if not is_wall(*l) and l[0] >=0 and l[1] >= 0]


graph = DynamicGridGraph()
explored = explore_nodes_in_step_limit(graph, (1, 1), 50)
print(len(explored))