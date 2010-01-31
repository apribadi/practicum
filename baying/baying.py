import heapq

c, n = map(int, raw_input().split())
a1, b1, d1 = map(int, raw_input().split())
a2, b2, d2 = map(int, raw_input().split())

def f1(c):
    return a1*c//d1+b1
def f2(c):
    return a2*c//d2+b2

queue = [c]
moos = []
seen = set([c])

for i in range(n):
    moo = heapq.heappop(queue)
    moos.append(moo)

    moo1 = f1(moo)
    if moo1 not in seen:
        heapq.heappush(queue, moo1)
        seen.add(moo1)

    moo2 = f2(moo)
    if moo2 not in seen:
        heapq.heappush(queue, moo2)
        seen.add(moo2)

print max(moos)

