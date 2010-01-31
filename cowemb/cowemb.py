import itertools

if __name__ == '__main__':
    n, r = map(int, raw_input().split())

    def line():
        a, b, c = map(int, raw_input().split())
        return (a, b, -c)
    lines = [line() for i in range(n)]

    def meet(line1, line2):
        a, b, e = line1
        c, d, f = line1
        return (e*d - b*f)**2 + (a*f - e*c)**2 <= r**2 * (d*b - b*c)**2

    total = sum(meet(*pair) for pair in itertools.product(lines, repeat=2))
    # to correct for overcounting and self-intersection
    print (total - n) / 2



