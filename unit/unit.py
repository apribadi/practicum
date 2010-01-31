import collections

n, m = map(int, raw_input().split())

conversions = collections.defaultdict(dict)
for i in range(n):
    c, unit1, unit2 = raw_input().split()
    conversions[unit1][unit2] = float(c)
    conversions[unit2][unit1] = 1 / float(c)

for i in range(m):
    start, end = raw_input().split()
    checked = set([start])
    current = {start: 1.0}
    while not current.has_key(end):
        next = dict()
        for unit, co in current.iteritems():
            for next_unit, mult in conversions[unit].iteritems():
                if next_unit not in checked:
                    next[next_unit] = co * mult
                    checked.add(next_unit)
        current = next
    print int(round(current[end] * 1000))
