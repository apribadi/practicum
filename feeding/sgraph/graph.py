'''Graphs

Graph
Digraph
'''

from collections import defaultdict, namedtuple
from itertools import chain, tee, izip

class SetView(object):
    def __init__(self, set_like):
        self.coll = set_like
    def __contains__(self, item):
        return item in self.coll
    def __iter__(self):
        return self.coll.__iter__()

class Digraph(object):
    '''A directed graph with a sparse representation

    Ex. 
    >>> dg = Digraph()
    >>> dg['a':'b'] = 1
    >>> ('a', 'b') in dg.edges()
    True
    >>> dg['a':'b']
    1
    >>> list(dg.children('a'))
    ['b']

    Don't mutate graph properties!
    '''

    class Node(object):
        def __init__(self, label, children, parents):
            self.label = label
            self.children = children
            self.parents = parents

    class Edge(object):
        def __init__(self, label):
            self.label = label

    def __init__(self):
        # representation
        self.nodedata = {}
        self.edgedata = {}

    def nodes(self):
        return SetView(self.nodedata)

    def edges(self):
        return SetView(self.edgedata)

    #
    # Nodes
    #

    def add_node(self, node):
        '''Add node.  
        
        Adding an already existing node is a no-op'''

        if node in self.nodedata:
            return
        self.nodedata[node] = self.Node(None, set(), set())

    def pop_node(self, node):
        '''Remove node and any connecting edges'''

        if node not in self.nodedata:
            return

        # remove edges
        c = self.nodedata[node].children
        p = self.nodedata[node].parents

        out_edges = ((node, other) for other in c)
        in_edges = ((other, node) for other in p)

        for edge in chain(out_edges, in_edges):
            self.edgedata.pop(edge, None)

        # remove node
        self.nodedata.pop(node)

    #
    # Edges
    #

    def add_edge(self, source, sink):
        '''Adds edges in a path

        If (a b c) is given, edges (a b) and (b c) are added.
        '''

        edge = (source, sink)

        if edge in self.edgedata:
            return

        # ensure nodes exist
        if source not in self.nodedata:
            self.add_node(source)
        if sink not in self.nodedata:
            self.add_node(sink)

        # add edge
        self.edgedata[edge] = self.Edge(None)
        self.nodedata[source].children.add(sink)
        self.nodedata[sink].parents.add(source)

    def pop_edge(self, source, sink):
        '''Remove edges in a path.
        
        If (a b c) is given, edges (a b) and (b c) are removed.
        '''

        edge = (source, sink)

        if edge not in self.edgedata:
            return

        self.nodedata[source].children.discard(sink)
        self.nodedata[sink].parents.discard(source)

        self.edgedata.pop(edge)

    #
    # Relations
    #

    def children(self, node):
        return SetView(self.nodedata[node].children)

    def parents(self, node):
        return SetView(self.nodedata[node].parents)

    #
    # labels
    #

    def __getitem__(self, key):
        if isinstance(key, slice):
            # edge
            edge = (key.start, key.stop)
            if edge not in self.edgedata:
                raise KeyError(key)
            return self.edgedata[edge].label
        else:
            # node
            if key not in self.nodedata:
                raise KeyError(key)
            return self.nodedata[key].label

    def __setitem__(self, key, val):
        if isinstance(key, slice):
            # edge
            source, sink = key.start, key.stop
            edge = (source, sink)

            if not edge in self.edgedata:
                self.add_edge(source, sink)
            self.edgedata[edge].label = val

        else:
            # node
            if key not in self.nodedata:
                self.add_node(key)
            self.nodedata[key].label = val

    #
    # Python utilities
    #

    def __repr__(self):
        return "<Digraph object>"

    def __str__(self):
        return self.__repr__()

    def __iter__(self):
        '''Iterate over nodes'''
        return self.nodedata.__iter__()

    def copy(self):
        other = Digraph()

        for node in self.nodes():
            other[node] = self[node]

        for source, sink in self.edges():
            other[source:sink] = self[source:sink]

        return other

if __name__ == '__main__':
    import doctest
    doctest.testmod()

