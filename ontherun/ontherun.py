if __name__ == '__main__':
    N, L = (int(s) for s in raw_input().split())
    clumps = [int(raw_input()) - L for i in xrange(N)]

    left = [abs(clump) for clump in clumps if clump < 0]
    right = [clump for clump in clumps if clump > 0]

    dp = dict()
    dp[0, 0] = 0
