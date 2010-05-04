from sgraph.graph import Digraph
from sgraph.algorithm import max_flow


def parse_line():
    return [eval(s) for s in raw_input().split()]

if __name__ == '__main__':
    n, t = parse_line()
    cow_prefs = []
    for i in range(n):
        fa, fb = parse_line()
        cow_prefs.append((fa - 1, fb - 1))

    g = Digraph()

    # to cows
    for cow_idx in xrange(n):
        cow = 'cow%d' % cow_idx
        g['source':cow] = 1

    # from food
    for food_idx in xrange(t):
        food = 'food%d' % food_idx
        g[food:'sink'] = 1

    # from cows to food
    for cow_idx in xrange(n):
        cow = 'cow%d' % cow_idx
        for food_idx in cow_prefs[cow_idx]:
            food = 'food%d' % food_idx
            g[cow:food] = 1

    print max_flow(g, 'source', 'sink')

