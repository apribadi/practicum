import collections

def ok(s, n):
    length = len(s)
    curr = val1 = val2 = s[:n].count('M')
    segment = collections.deque(s[:n])

    for i in range(length):
        if curr != val1 and curr != val2:
           if val1 == val2 and abs(val1 - curr) == 1:
               val2 = curr
           else:
               return False
        if segment.popleft() == 'M':
            curr -= 1
        next = s[(i + n) % length]
        if next == 'M':
            curr += 1
        segment.append(next)
    return True

if __name__ == '__main__':
    n = input()
    for i in range(n):
        s = raw_input()
        if all(ok(s, n) for n in range(1, len(s))):
            print 1
        else:
            print 0
