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

def solve():
    nodes = []

    source = 0
    sink = 1
    nodes.extend([source, sink])

    team_sizes = [int(s) for s in raw_input().split()]
    teams = dict((i + len(nodes), size) for (i, size) in enumerate(team_sizes))
    nodes.extend(teams.keys())

    table_sizes = [int(s) for s in raw_input().split()]
    tables = dict((i + len(nodes), size) for (i, size) in enumerate(table_sizes))
    nodes.extend(tables.keys())

    cap_matrix = defaultdict(lambda: defaultdict(lambda: 0))
    for team, size in teams.items():
        cap_matrix[source][team] = size

    for table, size in tables.items():
        cap_matrix[table][sink] = size

    for team in teams.keys():
        for table in tables.keys():
            cap_matrix[team][table] = 1

    seating = max_flow(nodes, cap_matrix, source, sink)

    if seating == sum(team_sizes):
        print 1
    else:
        print 0

if __name__ == '__main__':
    while raw_input().strip() != "0 0": # no, we don't need m, n
        solve()

