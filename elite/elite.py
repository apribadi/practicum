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
def dp(k, s, n):
    if s <= 0:
        return 0
    if n == 0:
        return 1
    if k == 0 and n != 0:
        return 0
    return dp(k - 1, s - k**2, n - 1) + dp(k - 1, s, n)
      
if __name__ == '__main__':
    n, s = (int(s) for s in raw_input().split())
    print dp(100, s, n)
