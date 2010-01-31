import itertools
import collections

# begin inserted algorithms

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
    path = cost_matrix.copy()

    for k, a, b in itertools.product(nodes, repeat=3):
        length = min(path.get(a + b, 100000000), 
                     path.get(a + k, 100000000) + path.get(k + b, 100000000))
        if length < 100000000:
            path[a + b] = length

    return path

PASS = 0
IMPASS = 1
BESSIE = 2
NI = 3
SHRUB = 4

if __name__ == '__main__':
    # Get input
    w, h = map(int, raw_input().split())
    def read_row():
        row = []
        while len(row) < w:
            row.extend(map(int, raw_input().split()))
        return row

    graph = [read_row() for i in range(h)]
    coords = list(itertools.product(range(h), range(w)))

    # find something in the graph
    def find(item):
        return [(row, col) for (row, col) in coords if graph[row][col] == item]

    clear = list(itertools.chain(*map(find, [PASS, BESSIE, SHRUB])))
    cow = find(BESSIE)[0]
    ni = find(NI)[0]
    shrubs = find(SHRUB)

    # adjacency matrix
    adj_matrix = dict()

    # Set up adj matrix
    for (a, b), (c, d) in itertools.product(clear, repeat=2):
        dr = abs(c - a)
        dc = abs(d - b)
        if dr + dc == 1:
            adj_matrix[(a, b, c, d)] = 1
            adj_matrix[(c, d, a, b)] = 1

    for (a, b) in clear:
        c, d = ni
        dr = abs(c - a)
        dc = abs(d - b)
        if dr + dc == 1:
            adj_matrix[(a, b, c, d)] = 1

    paths = floyd_warshall(coords, adj_matrix)

    def dist(shrub):
        d1 = paths.get(cow + shrub, 100000000)
        d2 = paths.get(shrub + ni, 100000000)
        return d1 + d2

    print min(dist(shrub) for shrub in shrubs)

