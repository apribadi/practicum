from collections import deque, defaultdict


# input
n, k, s = (int(s) for s in raw_input().split())
cows = [input() for i in range(n)]
pattern = tuple(input() for i in range(k))

# spot counts
count = {}
for cow in cows[:k]:
    if cow not in count:
        count[cow] = 1
    else:
        count[cow] += 1
cur = deque(cows[:k])

def pat():
    ranking = {}
    for rank, spots in enumerate(sorted(count.keys())):
        ranking[spots] = rank + 1
    return tuple(ranking[spots] for spots in cur)

here = 1
idxs = []
for cow in cows[k:]:
    # here is the 1-indexed of our current pattern
    if pat() == pattern:
        idxs.append(here)
        
    cur.append(cow)
    if cow not in count:
        count[cow] = 1
    else:
        count[cow] += 1

    out = cur.popleft()
    count[out] -= 1
    if count[out] == 0:
        del count[out]

    here += 1
if pat() == pattern:
    idxs.append(here)
    
    
print len(idxs)
for idx in idxs:
    print idx
