import itertools

n = input()

points = [raw_input().split() for i in range(n)]
points = [(int(x), int(y)) for x, y, z in points if z == 'Y']

first = min(points) # by x, then y

xmin = [(x, y) for x, y in points if x == first[0]]
rest = [(x, y) for x, y in points if x != first[0]]

def cotan(point):
    a, b = first
    x, y = point
    dx, dy = x - a, y - b
    return float(dy) / float(dx)

xmin.sort(key=lambda pt: -pt[1])
rest.sort(key=lambda pt: cotan(pt))

print len(points)

for pt in itertools.chain(xmin[-1:], rest, xmin[:-1]):
    x, y = pt
    print x, y
