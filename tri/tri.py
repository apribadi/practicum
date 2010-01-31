n = int(raw_input())

stacks = ([], [], [])

stacks[0].extend(reversed([int(raw_input()) for i in xrange(n)]))

print stacks
