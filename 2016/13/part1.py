from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

from lib.search import GridGraph, StartEndGraphProblem, dijkstra, reconstruct_path

is_wall = lambda x, y: bin((x * x + 3 * x + 2 * x * y + y + y * y) + int(puzzle_input_raw)).count("1") % 2 != 0

class DynamicGridGraph(GridGraph):
    def children(self, loc):
        # only return children which are not a well
        return [l for l in self.generate_4_neighbors(loc) if not is_wall(*l)]


graph = DynamicGridGraph()
problem = StartEndGraphProblem(graph, (1, 1), (31, 39))

paths, _ = dijkstra(problem)
shortest_path = reconstruct_path(paths, problem)
print(len(shortest_path) - 1)