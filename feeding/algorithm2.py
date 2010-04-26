'''A collection of algorithms
'''

import collections

class Graph(object):
    '''A directed graph with a sparse representation

    Stores edges, keyed by start and end nodes:
    >>> graph = Graph()
    >>> graph['a', 'b'] = 10
    >>> graph['a', 'b']
    10

    Accessing the parents or children of a node is fast:
    >>> list(graph.parents('b'))
    ['a']
    >>> list(graph.children('a'))
    ['b']
    '''
    def __init__(self, default=None):
        self._edges    = {}
        self._nodes    = set()
        self._children = collections.defaultdict(set)
        self._parents  = collections.defaultdict(set)
        self.default = default

    def _check_key(self, key):
        try:
            a, b = key
        except:
            raise KeyError("Key should be tuple (a, b) for an edge")
        return key

    def __getitem__(self, key):
        if key not in self._edges:
            if self.default is None:
                raise KeyError(key)
            self._edges[key] = self.default()
        return self._edges[key]


    def __setitem__(self, key, val):
        a, b = self._check_key(key)

        self._edges[key] = val

        self._nodes.add(a)
        self._nodes.add(b)

        self._children[a].add(b)
        self._parents[b].add(a)

    def __delitem__(self, key):
        a, b = self._check_key(key)

        del self._edges[key]

        self._children[a].remove(b)
        self._parents[b].remove(a)

        if not self._children[a]:
            del self._children[a]
        if not self._parents[b]:
            del self._parents[b]
        
        if a not in self._children and a not in self._parents:
            self._nodes.remove(a)
        if b not in self._children and b not in self._parents:
            self._nodes.remove(a)

    def __contains__(self, item):
        return item in self._edges

    def __iter__(self):
        return (key for key in self._edges)

    def __repr__(self):
        return "<Graph %s>" % repr(self._edges)

    def children(self, node):
        return (node for node in self._children[node])

    def parents(self, node):
        return (node for node in self._parents[node])

    def copy(self):
        cg = Graph()
        for key in self:
            cg[key] = self[key]
        return cg

def bfs(graph, source, sink):
    '''breadth-first search

    A clean implementation with an eye to efficiency for sparse graphs, using
    the provided Graph implementation.  
    '''
    queue = collections.deque([source])
    prevs = {source: None}

    while queue and sink not in prevs:
        cur = queue.popleft()
        adj = (b for b in graph.children(cur) if b not in prevs)
        for b in adj:
            prevs[b] = cur
            queue.append(b)

    if sink not in prevs:
        return None
    path, here = [], sink
    while here != source:
        path.append((prevs[here], here))
        here = prevs[here]
    return reversed(path)


def max_flow(graph, source, sink):
    '''max flow

    Uses the provided Graph implementation, where graph[a, b] is the (positive)
    capacity of the edge.
    '''
    resid = graph.copy()
    resid.default = lambda: 0
    total_flow = 0

    while True:
        path = bfs(resid, source, sink)
        if path is None:
            break
        path = list(path)
        flow = min(resid[a, b] for a, b in path)
        for a, b in path:
            resid[b, a] += flow
            resid[a, b] -= flow
            if resid[a, b] == 0:
                del resid[a, b]
        total_flow += flow

    return total_flow

if __name__ == '__main__':
    import doctest
    doctest.testmod()

