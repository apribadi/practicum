import collections
import itertools
import functools

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

@memoize
def dp(inc, val):
    if inc == 0 and val == 0:
        return 1
    elif inc <= 0 or val < 0:
        return 0
    else:
        return dp(inc, val - inc) + dp(inc - 1, val)
      
if __name__ == '__main__':
    n, k = (int(s) for s in raw_input().split())
    print dp(k, n)
