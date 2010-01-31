def graham(points):
    start = min(points, key=lambda (x, y): (y, x))
    q = sorted(points, key=lambda pt: cot(start, pt))
    q.remove(start)

    hull = [start, q[0]]
    pt1 = start
    for i in range(1, len(q)):
        pt2 = hull[-1]
        pt3 = q[i]
        hull.append
        pt2 = hull[-1]
        
        x3, y3 = q[i]
        if (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1) > 0:
            hull.pop()
            hull.append(pt3)
        else:
            
    for pt in q:
        
        
        
def cot(pt1, pt2):
    dx, dy = delta(pt2, pt1)
    if dy == 0:
        return 0
    else:
        return float(dx) / float(dy)

def delta(pt1, pt2):
    a, b = pt1
    c, d = pt2
    return (a - c, b - d)

def is_left(pt1, pt2, pt3):

