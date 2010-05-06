import os.path as path
import sys

base = path.abspath('..')
sys.path.insert(0, path.join(base, 'sgraph'))

# source for sgraph at http://github.com/apribadi/sgraph
from sgraph.graph import Digraph
from sgraph.algorithm import dijkstra

from itertools import product
from operator import add


def parse_line():
    return [eval(s) for s in raw_input().split()]

if __name__ == '__main__':
    v, r, c = parse_line()
    elev = [[int(s) for s in raw_input().split()] for row in xrange(r)]

    def velo(pt):
        row, col = pt
        return float(v * (2 ** (elev[0][0] - elev[row][col])))
    def vec_add(u, v):
        return tuple(map(add, u, v))

    points = [(row, col) for row, col in product(range(r), range(c))]
    deltas = [[-1, 0], [0, -1], [0, 1], [1, 0]]

    graph = Digraph()

    for pt, delta in product(points, deltas):
        adj = vec_add(pt, delta)
        if 0 <= adj[0] < r and 0 <= adj[1] < c:
            graph[pt:adj] = 1 / velo(pt)

    print "%.2f" % dijkstra(graph, (0,0), (r - 1, c - 1))
