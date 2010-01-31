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
    resid_matrix = [[cap_matrix[a][b] for b in nodes] for a in nodes]
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

def solve(n, f, d, cows):
    # Give labels to the nodes
    nodes = []

    def new_nodes(num):
        prev = len(nodes)
        ns = range(prev, prev + num)
        nodes.extend(ns)
        return ns

    source, sink = new_nodes(2)
    foods = new_nodes(f)
    drinks = new_nodes(d)
    cows1 = new_nodes(n)
    cows2 = new_nodes(n)

    # Set up the graph
    cap_matrix = [[0 for b in nodes] for a in nodes]

    for food in foods:
        cap_matrix[source][food] = 1
    for drink in drinks:
        cap_matrix[drink][sink] = 1

    for c1, c2 in zip(cows1, cows2):
        cap_matrix[c1][c2] = 1

    for cow, matched_foods in zip(cows1, zip(*cows)[0]):
        fs = [foods[i - 1] for i in matched_foods]
        for food in fs:
            cap_matrix[food][cow] = 1

    for cow, good_drinks in zip(cows2, zip(*cows)[1]):
        ds = [drinks[i - 1] for i in good_drinks]
        for drink in ds:
            cap_matrix[cow][drink] = 1

    # See how many cows eat.
    fed = max_flow(nodes, cap_matrix, source, sink)

    return fed

if __name__ == '__main__':
    n, f, d = map(int, raw_input().split())

    cows = []
    for i in range(n):
        xs = map(int, raw_input().split())
        foods = xs[2 : 2 + xs[0]]
        drinks = xs[2 + xs[0] : 2 + xs[0] + xs[1]]
        cows.append((foods, drinks))

    print solve(n, f, d, cows)


