"""
This module contains systematic search algorithm
implemtations, like Breadth-First-, Depth-First-Searches.
The ``lib.search.graph.Graph`` is supposed to be
used as a bases for these implementations.
"""

import sys
from typing import Callable, Dict, List
from collections import deque
import itertools

from lib.search.graph import Graph, Location
from lib.search.priorityqueue import PriorityQueue


def dijkstra(problem):
    frontier = PriorityQueue()
    frontier.append(problem.start, 0)

    came_from: Dict[Location, Optional[Location]] = {}
    cost_so_far: Dict[Location, float] = {}
    came_from[problem.start] = None
    cost_so_far[problem.start] = 0
    
    while frontier:
        current: Location = frontier.pop()
        
        if problem.goal_test(current):
            break
        
        for child in problem.actions(current):
            new_cost = problem.path_cost(cost_so_far[current], current, child)
            if child not in cost_so_far or new_cost < cost_so_far[child]:
                cost_so_far[child] = new_cost
                frontier.append(child, new_cost)
                came_from[child] = current
    
    return came_from, cost_so_far


def reconstruct_path(came_from: Dict[Location, Location], problem) -> List[Location]:
    current: Location = problem.goal
    path: List[Location] = []
    while current != problem.start:
        path.append(current)
        current = came_from[current]
    path.append(problem.start)
    path.reverse()
    return path


def explore_nodes_in_step_limit(graph, start: Location, limit):
    frontier = deque([start])
    new_frontier = frontier.copy()
    explored = {start}
    steps = 0

    while steps < limit:
        frontier = new_frontier.copy()
        new_frontier = deque()
        while frontier:
            node = frontier.pop()
            for child in graph.children(node):
                if child not in explored:
                    explored.add(child)
                    new_frontier.append(child)

        steps += 1

    return explored


def generic_bfs(problem):
    stack = deque([(problem.start, tuple())])
    paths = set()
    while stack:
        position, path = stack.popleft()
        if problem.goal_test(position):
            paths.add(path)
            continue

        stack.extend(problem.actions(position, path))
        
    return paths


def travelling_salesman_problem(graph, start, roundtrip=False): 

    vertices_without_start = [v for v in graph.edges if v != start]
    min_path = None
    min_path_cost = sys.maxsize

    for path in itertools.permutations(vertices_without_start):
        path_cost = 0
        previous_node = start
        for current_node in path:
            path_cost += graph.cost(previous_node, current_node)
            previous_node = current_node

        if roundtrip:
            path_cost += graph.cost(previous_node, start)
        
        if path_cost < min_path_cost:
            min_path_cost = path_cost
            min_path = path

    return min_path, min_path_cost