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
    nodes, path = copy_path_matrix(nodes, cost_matrix)

    for k, a, b in itertools.product(nodes, repeat=3):
        path[a][b] = min(path[a][b], path[a][k] + path[k][b])

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

    clear = list(itertools.chain(*map(find, [PASS, BESSIE, NI, SHRUB])))

    # adjacency matrix
    adj_matrix = [[1000000000 for aa in coords] for bb in coords]
    def to_index(a, b):
        return a * w + b
    def to_coord(index):
        return (index // w, index % w)

    # Set up adj matrix
    for (a, b), (c, d) in itertools.product(clear, repeat=2):
        dr = abs(c - a)
        dc = abs(d - b)
        if dr + dc == 1:
            adj_matrix[to_index(a, b)][to_index(c, d)] = 1
            adj_matrix[to_index(c, d)][to_index(a, b)] = 1

    cow = find(BESSIE)[0]
    ni = find(NI)[0]
    shrubs = find(SHRUB)

    paths = floyd_warshall(range(len(coords)), adj_matrix)

    def dist(shrub):
        d1 = paths[to_index(*cow)][to_index(*shrub)]
        d2 = paths[to_index(*shrub)][to_index(*ni)]
        return d1 + d2

    print min(dist(shrub) for shrub in shrubs)

