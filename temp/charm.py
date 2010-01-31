import collections
import itertools
import functools
import sys

sys.setrecursionlimit(1000000000)

def memoize(function):
    cache = {}
    @functools.wraps(function)
    def wrapper(*args):
        if args in cache:
            return cache[args]
        else:
            rv = function(*args)
            cache[args] = rv
            return rv
    return wrapper
      
if __name__ == '__main__':
    n, m = (int(s) for s in raw_input().split())
    weight = []
    value = []
    for i in xrange(n):
        w, v = (int(s) for s in raw_input().split())
	weight.append(w)
	value.append(v)

    def dp(j, Y):
        if j == 0:
            return 0
        if Y == 0:
            return 0

        if weight[j - 1] > Y:
            return dp(j - 1, Y)
        else:
            return max(dp(j - 1, Y), value[j - 1] + dp(j - 1, Y - weight[j - 1]))
    dp = memoize(dp)

    print dp(n, m)

