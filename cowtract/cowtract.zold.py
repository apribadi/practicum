import collections

def solve():
    n, m = map(int, raw_input().split())
    links = collections.defaultdict(dict)
    for i in range(m):
        a, b, c = map(int, raw_input().split())
        links[a][b] = links[b][a] = c

    reached = set([1])
    left = set(range(2, n+1))
    current = [1]
    cost = 0
    while left:
        links_out = [(links[a][b], b) 
                       for a in current 
                       for b in links[a].keys()
                       if b not in reached]
        if not links_out:
            return -1
        c, b = max(links_out)
        reached.add(b)
        left.remove(b)
        current.append(b)
        cost += c
    return cost

if __name__ == '__main__':
    print solve()

