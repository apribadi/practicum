import itertools

rcount, ccount = map(int, raw_input().split())
scene = {}

for r in range(rcount):
    line = raw_input()
    for c, char in zip(range(ccount), line):
        scene[r, c] = char

def vadd(u, v):
    return tuple(map(lambda x, y: x + y, u, v))

def in_bounds(u):
    r, c = u
    return 0 <= r < rcount and 0 <= c < ccount

deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]

def do():
    joe = set()
    passable = set()
    fire = set()

    for loc in scene.iterkeys():
        thing = scene[loc]
        if thing != '#' and thing != 'F':
            passable.add(loc)
        if thing == 'J':
            joe.add(loc)
        if thing == 'F':
            fire.add(loc)

    for time in itertools.count(1):
        # set things on fire
        newfire = set()
        for here in fire:
            for d in deltas:
                there = vadd(here, d)
                if in_bounds(there):
                    newfire.add(there)
                    passable.discard(there)
        fire.update(newfire)


        # move joe
        newjoe = set()

        for here in joe:
            for d in deltas:
                there = vadd(here, d)
                if not in_bounds(there):
                    # escape !
                    return time
                if there in passable:
                    newjoe.add(there)

        joe = newjoe

        # we're dead
        if not joe:
            return None
        
res = do()
if res is None:
    print 'IMPOSSIBLE'
else:
    print res
