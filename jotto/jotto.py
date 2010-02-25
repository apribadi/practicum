#!/usr/bin/python

import string

soph  = {
    'chalk': 1, 
    'quine': 1,
    'aught': 2,
    'jotto': 2,
    'savvy': 2,
    'clash': 2 
    }

jrs   = {
    'chalk': 0,
    'quine': 1,
    'aught': 1,
    'jotto': 2,
    'savvy': 0,
    'clash': 0
    }

srs   = {
    'chalk': 1, 
    'quine': 2,
    'aught': 1,
    'jotto': 0,
    'savvy': 1,
    'clash': 1
    }

other = {
    'chalk': 1, 
    'quine': 2,
    'aught': 2,
    'jotto': 1,
    'savvy': 1,
    'clash': 1
    }

classes = {
    'sophomores': soph, 
    'juniors': jrs, 
    'seniors': srs, 
    'other': other
    }

def stat(word):
    return tuple(word.count(c) for c in string.ascii_lowercase)

def score(stat1, stat2):
    return sum(map(min, stat1, stat2))

data = {}
with open('words.txt', 'r') as f:
    for line in f:
        word = line.strip()
        data[word] = stat(word)


def solve(word_map):
    possible = []
    for word1, word1_stat in data.iteritems():
        if all(score(word1_stat, stat(word2)) == expected
               for word2, expected
               in word_map.iteritems()):
            possible.append(word1)
    return possible

for class_name, word_map in classes.iteritems():
    print "For %s, the possible words are:" % class_name
    print
    words = solve(word_map)
    for word in words:
        print word
    print

