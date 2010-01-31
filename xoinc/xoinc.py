import cProfile
import collections
import numpy
        
def main():
    # input
    n = int(raw_input())
    coins = [int(raw_input()) for i in xrange(n)]

    # the ith elem of cum is the total value of the last i coins
    cum_sum = [0]
    for coin in reversed(coins):
        cum_sum.append(cum_sum[-1] + coin)

    # our large array
    #dp = numpy.zeros([n + 1, n + 1], dtype = numpy.int64)
    dp = [[0 for i in xrange(n+1)] for j in xrange(n+1)]

    # fill it in
    for coins_left in xrange(1, n + 1):
        diag = [dp[coins_left - to_take][to_take] for to_take in xrange(coins_left, 0, -1)]
        least = diag[-1]
        total_monies = cum_sum[coins_left]
        for taken_last in xrange(1, n - coins_left + 2):
            if not diag:
                dp[coins_left][taken_last] = total_monies
                continue
            least = min(least, *diag[-2:])
            try:
                diag.pop()
                diag.pop()
            except IndexError:
                pass
            dp[coins_left][taken_last] = total_monies - least

    # and our answer is here
    print dp[n][1]

if __name__ == '__main__':
    cProfile.run("main()")
    #main()


