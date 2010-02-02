import collections

def int_line():
    return [int(s) for s in raw_input().split()]

n, c = int_line()

# val -> count
coins = dict(int_line() for i in xrange(n))

buckets = 0
while coins:
    needed = c
    for coin in sorted(coins.keys(), reverse=True):
        if coin <= needed:
            left = coins[coin]
            to_take = min(needed // coin, left)
            if to_take == left:
                del coins[coin]
            else:
                coins[coin] -= to_take
            needed -= to_take * coin

    if needed > 0:
        if not coin:
            break
        coin = min(coins.keys())
        if coins[coin] == 1:
            del coins[coin]
        else:
            coins[coin] -= 1
    buckets += 1

print buckets
