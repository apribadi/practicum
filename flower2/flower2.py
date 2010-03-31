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

destroyed = [0]

def step(time):
    for cow in cows:
        destroyed[0] += cow.damage * time

cows.sort(key=lambda c: c.badness)

while cows:
    cow = cows.pop()
    time = 2 * cow.time
    step(time)
        
    
print destroyed[0]

