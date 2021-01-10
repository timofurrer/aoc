"""
This module provies common implementation
of Search problems. These problem implementation
can be used with the search optimization 
algorithms in ``lib.search``.
"""

from lib.search.graph import Location, Graph


class StartEndGraphProblem:
    """The implementation of a problem to find "a" path from a start to end."""

    def __init__(self, graph: Graph, start: Location, goal: Location):
        self.graph = graph
        self.start = start
        self.goal = goal

    def goal_test(self, state):
        """Return True if the state is a goal. The default method compares the
        state to self.goal or checks for state in self.goal if it is a
        list, as specified in the constructor. Override this method if
        checking against a single self.goal is not enough."""
        return state == self.goal

    def actions(self, loc: Location):
        """The actions at a graph node are just its neighbors."""
        return self.graph.children(loc)

    def path_cost(self, cost_so_far, from_loc: Location, to_loc: Location):
        return cost_so_far + (self.graph.cost(from_loc, to_loc) or float("inf"))