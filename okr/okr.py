def prefixes(s):
    return (s[:i] for i in range(len(s) + 1))
def proper_prefixes(s):
    return (s[:i] for i in range(len(s)))

if __name__ == '__main__':
    raw_input() # discard
    s = raw_input().strip()
    count = 0
    for prefix in prefixes(s):
        periods = [x for x in proper_prefixes(prefix) if (x * 2).startswith(prefix)]
        if periods:
            count += max(map(len, periods))
    print count
