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
    weight = [0]
    value = [0]
    for i in xrange(n):
        w, v = (int(s) for s in raw_input().split())
	weight.append(w)
	value.append(v)

    dp = [[0 for i in range(m + 1)] for j in range(n + 1)]
    

    for j in range(1, n + 1):
        for Y in range(1, m + 1):
	    if weight[j] > Y:
	        dp[j][Y] = dp[j - 1][Y]
	    else:
	        dp[j][Y] = max(dp[j - 1][Y], value[j] + dp[j - 1][Y - weight[j]])

    print dp[n][m]

