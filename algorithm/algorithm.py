"""
A collection of graph algorithms.
"""

import collections
import itertools

__all__ = ['floyd_warshall', 'bfs', 'max_flow', 'kruskal']

def copy_path_matrix(nodes, matrix):
    """
    Takes a list of nodes and a matrix double-indexed by those nodes, and
    returns the integers 0 to n - 1 as the nodes and a new dense array for the
    matrix.
    """
    matrix = [[matrix[a][b] for b in nodes] for a in nodes]
    nodes = range(len(nodes))
    return nodes, matrix

def floyd_warshall(nodes, cost_matrix):
    """
    Takes a list of nodes and a cost matrix (with high values as impassible) and
    returns a dense matrix of node to node path lengths indexed by the integers
    0 to n - 1.
    """
    nodes, path = copy_path_matrix(nodes, cost_matrix)

    for k, a, b in itertools.product(nodes, repeat=3):
        path[a][b] = min(path[a][b], path[a][k] + path[k][b])

    return path

def bfs(nodes, cap_matrix, source, sink):
    """
    Takes a list of nodes and a capacity matrix, with capacity > 0 being
    passable, along with the source and sink.  Returns a list of (node, node)
    tuples representing a path from source to sink.
    """
    queue = collections.deque([source])
    paths = {source: []}

    while queue:
        curr = queue.popleft()
        for next in nodes:
            if cap_matrix[curr][next] > 0 and next not in paths:
                paths[next] = paths[curr] + [(curr, next)]
                queue.append(next)
        if sink in paths:
            return paths[sink]

def max_flow(nodes, cap_matrix, source, sink):
    """
    Takes a list of nodes, capacity matrix, source, and sink, and returns the
    maximum flow from source to sink.
    """
    nodes, resid = copy_path_matrix(nodes, cap_matrix)
    total_flow = 0

    while True:
        path = bfs(nodes, resid, source, sink)
        if not path:
            break
        flow = min(resid[a][b] for (a, b) in path)
        for a, b in path:
            resid[a][b] -= flow
            resid[b][a] += flow
        total_flow += flow

    return total_flow

def kruskal(nodes, links):
    """
    Takes a list of nodes and undirected links in a list of (node, node, cost)
    tupes.  Returns a list of (node, node, cost) representing a minimal cost
    spanning tree.
    """
    links = sorted(links, key=lambda x: -x[2])
    forest = dict((node, set([node])) for node in nodes)
    span = []
    for a, b, c in links:
        if a not in forest[b]:
            tree = forest[a].union(forest[b])
            forest.update((node, tree) for node in tree)
            span.append((a, b, c))

    if len(span) == len(nodes) - 1:
        return span

