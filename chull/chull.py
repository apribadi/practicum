import itertools

n = input()

points = [raw_input().split() for i in range(n)]
points = [(int(x), int(y)) for x, y, z in points if z == 'Y']

first = min(points, key=lambda pt: (pt[1], pt[0]))

ymin = [(x, y) for x, y in points if y == first[1]]
rest = [(x, y) for x, y in points if y != first[1]]

def cotan(point):
    a, b = first
    x, y = point
    dx, dy = x - a, y - b
    return float(dx) / float(dy)

ymin.sort(key=lambda pt: pt[0])
rest.sort(key=lambda pt: -cotan(pt))

for pt in itertools.chain(ymin, rest):
    x, y = pt
    print x, y
