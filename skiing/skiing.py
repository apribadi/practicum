import os.path as path
import sys

sys.path.insert(0, '/home/apribadi/09-10/practicum/sgraph')

# source for sgraph at http://github.com/apribadi/sgraph
from sgraph.graph import Digraph
from sgraph.algorithm import dijkstra

from itertools import product


def parse_line():
    return [eval(s) for s in raw_input().split()]

if __name__ == '__main__':
    v, r, c = parse_line()
    elevs = [[int(c) for c in raw_input()] for row in xrange(r)]
    initelev = elevs[0][0]

    def velo(point):
        row, col = point
        return v * (2 ** (initelev - elevs[row][col]))
    points = product(range(r), range(c))
    velos = dict((pt, velo(pt)) for pt in points)

    def vec_add(u, v):
        
    deltas = [[-1, 0], [0, -1], [0, 1], [1, 0]]

    for pt, delta in product(points, deltas):
        vec_add
    ]
    (printf "%.2f\n" (if (= best inf) 0.00 (float best)))
    (shutdown-agents)))

(main)
