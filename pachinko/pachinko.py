if __name__ == '__main__':
    r = input()
    tri = [map(int, raw_input().split()) for i in range(r)]
    for row in range(r, 0, -1): # r to 1
       for col in range(row - 1):
            tri[row - 2][col] += max(tri[row - 1][col], tri[row - 1][col + 1])
    print tri[0][0]

