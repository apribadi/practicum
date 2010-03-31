import itertools

n = input()

points = []
count = 0
for i in range(n):
    x, y, z = raw_input().split()
    if z == 'Y':
        point = (int(x), int(y))
        points.append(point)
        count += 1

first = min(points, key=lambda pt: tuple(reversed(pt)))
ymin = first[1]

ymins = filter(lambda pt: pt[1] == ymin, points)
points = filter(lambda pt: pt[1] != ymin, points)

def cotan(point):
    a, b = first
    x, y = point
    dx, dy = x - a, y - b
    return float(dx) / float(dy)

ymins.sort(key=lambda pt: pt[0])
points.sort(key=lambda pt: -cotan(pt))

for pt in itertools.chain(ymins, points):
    x, y = pt
    print x, y
