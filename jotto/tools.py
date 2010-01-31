#! /usr/local/bin/python

import string
import glob

def score(word1, word2):
    def letters(word):
        return tuple(word.count(letter) 
                     for letter 
                     in string.ascii_lowercase)
    return sum(map(min, letters(word1), letters(word2)))

def gen():
    out = open('words.txt', 'w') 
    names = (glob.glob('american-words.*') +
             glob.glob('english-words.*'))
    for name in names:
        with open(name, 'r') as f:
            words = [line.strip() for line in f]
            for word in filter(lambda x: len(x) == 5, words):
                out.write(word)
                out.write('\n')
    out.close()

def gen2():
    name = 'ispell.words'
    out = open('words.txt', 'w') 
    with open(name, 'r') as f:
        words = [line.strip() for line in f]
        for word in filter(lambda x: len(x) == 5, words):
            out.write(word)
            out.write('\n')
    out.close()

def pick():
    with open('words.txt', 'r') as f:
        words = [line.strip() for line in f]

    possibles = ['apron', 'droop', 'knots', 'prank', 'prong']

    def fun(word1):
        scores = [score(word1, word2) for word2 in possibles]
        num_vals = sum(num in scores for num in range(0, 6))
        return num_vals >= 5

    print filter(fun, words)

if __name__ == '__main__':
    gen()

