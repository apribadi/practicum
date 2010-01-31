import collections
import itertools

if __name__ == '__main__':
    n, k = (int(s) for s in raw_input().split())

    dp = collections.defaultdict(lambda: 0)
    dp[0, 0] = 1

    for inc, val in itertools.product(xrange(1, k + 1), xrange(n + 1)):
        dp[inc, val] = dp[inc, val - inc] + dp[inc - 1, val]

    print dp[k, n]


