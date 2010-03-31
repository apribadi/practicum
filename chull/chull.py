n = input()

points = []
count = 0
for i in range(n):
    x, y, z = raw_input().split()
    point = map(int, [x, y])
    if z == 'Y':
        points.append(point)
        count += 1

first = min(points, key=lambda (x, y): (y, x))
ymin = first[1]

ymins = filter(lambda pt: pt[1] == ymin, points)
points = filter(lambda pt: pt[1] != ymin, points)

def cotan(point):
    a, b = first
    x, y = point
    dx, dy = x - a, y - b
    return float(dx) / float(dy)

points.sort(key=lambda pt: -cotan(pt))

def ppt(point):
    x, y = point
    print x, y

print count
for point in sorted(ymins, key=lambda pt: pt[0]):
    ppt(point)
for point in points:
    ppt(point)

