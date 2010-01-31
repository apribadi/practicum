import collections

class Node(object):
    def __init__(self):
        self.data = collections.defaultdict(Node)
        self.is_end = False

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def __contains__(self, key):
        return key in self.data

if __name__ == '__main__':
    s = raw_input()
    n = input()
    words = [raw_input() for i in range(n)]

    root = Node()

    # initialize word tree
    for word in words:
        node = root
        for c in word:
            node = node[c]
        node.is_end = True

    # parse input
    states = [(root, '')]
    for c in s:
        ended = [part for (node, part) in states if node.is_end]
        if ended:
            states.append((root, ended[0] + ' '))
        states = [(node[c], part + c) for (node, part) in states if c in node]
        if sum(node.is_end for (node, part) in states) > 1:
            states = filter(lambda (node, part): not node.is_end, states)

    # check if we have an answer
    good = [part for (node, part) in states if node.is_end]
    if len(good) == 1:
        print good[0]
    else:
        print "ambiguous"

