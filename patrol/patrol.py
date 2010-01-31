from collections import defaultdict, deque

def bfs(nodes, cap_matrix, source, sink):
    queue = deque([source])
    paths = {source: []}

    while queue:
        curr = queue.pop()
        for next in nodes:
            if cap_matrix[curr][next] > 0 and next not in paths:
                paths[next] = paths[curr] + [(curr, next)]
                queue.appendleft(next)
        if sink in paths:
            return paths[sink]

def max_flow(nodes, cap_matrix, source, sink):
    # convert to use integers from 0 to n - 1
    resid_matrix = [[cap_matrix[a][b] for b in nodes] for a in nodes]
    nodes = range(len(nodes))

    total_flow = 0

    while True:
        path = bfs(nodes, resid_matrix, source, sink)
        if not path:
            break
        flow = min(resid_matrix[a][b] for (a, b) in path)
        for (a, b) in path:
            resid_matrix[a][b] -= flow
            resid_matrix[b][a] += flow
        total_flow += flow

    return total_flow

if __name__ == '__main__':
    n, m = map(int, raw_input().split())
    def line():
        a, b = map(int, raw_input().split())
        return (a, b)
    links = [line() for i in range(m)]
    nodes = range(1, n + 1)

    cap_matrix = [[0 for b in range(n + 1)] for a in range(n + 1)]

    for a, b in links:
        cap_matrix[a][b] = 1
        cap_matrix[b][a] = 1

    print max_flow(nodes, cap_matrix, 1, n)


