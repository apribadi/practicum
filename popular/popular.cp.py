import collections
import itertools

if __name__ == '__main__':
    n, m = map(int, raw_input().split())
    links = collections.defaultdict(list)
    for i in range(m):
        a, b = map(int, raw_input().split())
        links[b].append(a)

    count = 0
    everything = set(range(1, n + 1))
    for source in range(1, n + 1):
        reached = set([source])
        current = [source]
        while len(current) != 0:
            next = []
            for cow in itertools.chain(*(links[a] for a in current)):
                if cow not in reached:
                    next.append(cow)
                    reached.add(cow)
            current = next
        if reached == everything:
            count += 1
    print count
            

