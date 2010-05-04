'''A collection of graph algorithms

Currently assumes directed graphs.
'''

from collections import deque

def floyd_warshall(graph):
    '''All-pairs shortest paths
    '''
    pass

def bfs(graph, source, sink):
    '''Return a list of edges from source to sink, or None.
    
    Requires
        graph.children
        edge labels
    '''
    queue = deque([source])
    prevs = {source: None}

    while queue:
        cur = queue.popleft()
        if cur == sink:
            break
        for b in graph.children(cur):
            if b not in prevs:
                prevs[b] = cur
                queue.append(b)
    else:
        return None

    path, here = [], sink
    while here != source:
        path.append((prevs[here], here))
        here = prevs[here]
    return list(reversed(path))


def max_flow(graph, source, sink):
    '''Max flow of digraph
    
    Requires
        digraph.copy
        digraph.children
        edge labels
    '''
    resid = graph.copy()
    total_flow = 0

    while True:
        path = bfs(resid, source, sink)
        if path is None:
            break

        flow = min(resid[a:b] for a, b in path)

        for a, b in path:
            if (b, a) not in resid.edges():
                resid[b:a] = 0
            resid[a:b] -= flow
            resid[b:a] += flow
            if resid[a:b] == 0:
                resid.pop_edge(a, b)

        total_flow += flow

    return total_flow

