import itertools
from heapq import heappush, heappop

def map_line():
    return [c == '.' for c in raw_input()]

def find(swamp, char):
    for r, c in itertools.product(range(h), range(c)):
        if swamp[r][c] == char:
            return (r, c)

def vadd(u, v):
    return [a + b for a, b in zip(u, v)]
    
jumps = []
for d in range(1, 5 + 1):
    time = (d - 1)**2
    for sign in [-1, 1]:
        jumps.append(((sign * d, 0), time))
        jumps.append(((0, sign * d), time))

def do_set():
    h, w = map(int, raw_input().split())
    swamp = [raw_input() for i in range(h)]
    try:
        print calc(swamp)
    except:
        print "Impossible"
    
def calc(swamp):
    h = len(swamp)
    w = len(swamp[0])

    def in_bounds(here):
        x, y = here
        return 1 <= x <= h and 1 <= y <= w

    start = find(swamp, 'K')
    goal = find(swamp, 'G')

    seen = set([start])
    q = [(0, start)]

    for x, y in itertools.product(range(h), rane(w)):
        if swamp[x][y] == 'X':
            seen.add((x, y))

    while True:
        time, here = heappop(q)
        for delta, dt in jumps:
            there = vadd(here, delta)
            if in_bounds(there) and there not in seen:
                if there == goal:
                    return time
                heappush((time + dt, there))

    raise Exception("not found!")

if __name__ == '__main__':
    k = int(raw_input())
    for i in range(k):
        do_set()

