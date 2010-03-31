class Cow(object):
    def __init__(self, time, damage):
        self.time = time
        self.damage = damage

    @property
    def badness(self):
        return float(self.damage) / float(self.time)
        
n = input()
cows = []
for i in range(n):
    t, d = (int(s) for s in raw_input().split())
    cows.append(Cow(t, d))

cows.sort(key=lambda c: c.badness)

destroyed = 0
damage = sum(cow.damage for cow in cows)
while cows:
    cow = cows.pop()
    time = 2 * cow.time
    damage -= cow.damage
    destroyed += time * damage
    
print destroyed

