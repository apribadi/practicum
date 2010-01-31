import itertools
import collections

def bfs(nodes, cap_matrix, source, sink):
    queue = collections.deque([source])
    paths = {source: []}

    while queue:
        curr = queue.popleft()
        for next in nodes:
            if cap_matrix[curr, next] > 0 and next not in paths:
                paths[next] = paths[curr] + [(curr, next)]
                queue.append(next)
        if sink in paths:
            return paths[sink]

if __name__ == '__main__':
    # input
    n, e = map(int, raw_input().split())
    g = [raw_input() for i in range(2 * n - 1)]

    # initialize stuff
    PATH_CHARS = '-|'
    START = 'S'
    END = 'E'

    nodes = list(itertools.product(range(n), range(e)))
    adj_matrix = collections.defaultdict(lambda: 0)

    def adjacent(node1, node2):
        (a, b), (c, d) = node1, node2
        return abs(c - a) + abs(d - b) == 1

    def connected(node1, node2):
        (a, b), (c, d) = node1, node2
        return g[a + c][b + d] in PATH_CHARS

    # make adjacency matrix
    for node1, node2 in itertools.product(nodes, repeat=2):
        if adjacent(node1, node2) and connected(node1, node2):
            adj_matrix[node1, node2] = 1

    def find(item):
        return ((a, b) for a, b in nodes if g[2 * a][2 * b] == item).next()

    path = bfs(nodes, adj_matrix, find(START), find(END))

    # translate the path into directions
    def find_heading(step):
        (a, b), (c, d) = step
        return (c - a, d - b)

    def heading_char(heading):
        d = {(0, -1): 'W', (0, 1): 'E', (-1, 0): 'N', (1, 0): 'S'}
        return d[heading]

    for heading, steps in itertools.groupby(path, find_heading):
        print heading_char(heading), len(list(steps))

