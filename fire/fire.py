import itertools

rcount, ccount = map(int, raw_input().split())
scene = {}

joe = set()
passable = set()
fire = set()

for r in range(rcount):
    line = raw_input()
    for c, char in zip(range(ccount), line):
        loc = (r, c)
        if char != '#' and char != 'F':
            passable.add(loc)
        if char == 'J':
            joe.add(loc)
        if char == 'F':
            fire.add(loc)

def vadd(u, v):
    a, b = u
    x, y = v
    return (a + x, b + y)

def in_bounds(u):
    r, c = u
    return 0 <= r < rcount and 0 <= c < ccount

deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]
def adjacent(here):
    return [vadd(here, d) for d in deltas]


def do(joe, fire, passable):
    for time in itertools.count(1):
        # set things on fire
        newfire = set()
        for here in fire:
            for there in adjacent(here):
                if there in passable:
                    newfire.add(there)
                    passable.remove(there)
        fire = newfire

        # move joe
        newjoe = set()
        for here in joe:
            for there in adjacent(here):
                if not in_bounds(there):
                    # escape !
                    return time
                if there in passable:
                    newjoe.add(there)
        joe = newjoe

        # we're dead
        if not joe:
            return None
        
res = do(joe, fire, passable)
if res is None:
    print 'IMPOSSIBLE'
else:
    print res
