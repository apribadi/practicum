n, k, m, r = (int(x) for x in raw_input().split())
cs = []
ps = []
for i in range(n):
    c, p = (int(x) for x in raw_input().split())
    cs.append(c)
    ps.append(p)

row = {0: 0}
for cost, prod in zip(cs, ps):
    for total_cost, max_prod in row.copy().iteritems():
        row[total_cost + cost] = max(row.get(total_cost + cost, 0), max_prod + prod)


es = [m - float(cost) / float(prod) for cost, prod in row.iteritems() if cost <= r and prod >= k]

try:
    best = max(es)
except:
    print -1
if best > 0:
    print int(best * 1000)
else:
    print -1
