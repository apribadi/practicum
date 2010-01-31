from heapq import heappush, heappop, heapify

if __name__ == '__main__':
    n, t = map(int, raw_input().split())

    # song: [-rating, index]
    song_queue = []
    song_dict = {}
    for i in range(n):
        song = [-int(raw_input()), i]
        song_queue.append(song)
        song_dict[i] = song
    # heapify(song_queue)
    song_queue.sort()

    played = []
    for i in range(t):
        #song = heappop(song_queue)
        song = song_queue[0]

        index = song[1]
        rating = -song[0]  # ratings are stored negative
        played.append(index)

        # modify ratings
        song[0] = 0        # set to zero
        inc = rating // (n - 1)
        rem = rating % (n - 1)
        for key in song_dict.keys():
            if key != index:
                song_dict[key][0] -= inc
        key = 0
        while rem:
            if key != index:
                song_dict[key][0] -= 1
                rem -= 1
            key += 1
        
        #heappush(song_queue, song)
        song_queue.sort()
        
    for song in played:
        print song + 1
