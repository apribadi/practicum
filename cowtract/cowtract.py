import collections

def kruskal(nodes, links):
    links = sorted(links, key=lambda x: -x[2])
    forest = dict((node, set([node])) for node in nodes)
    span = []
    for (a, b, c) in links:
        if a not in forest[b]:
            tree = forest[a].union(forest[b])
            for node in tree:
                forest[node] = tree
            span.append((a, b, c))

    if forest.popitem()[1] == set(nodes):
        return span

if __name__ == '__main__':
    n, m = map(int, raw_input().split())

    def line():
        a, b, c = map(int, raw_input().split())
        return (a, b, c)
    links = [line() for i in range(m)]

    span = kruskal(range(1, n + 1), links)

    if span:
        print sum(c for (a, b, c) in span)
    else:
        print -1
