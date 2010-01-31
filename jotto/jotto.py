#! /usr/local/bin/python

import string

def score(word1, word2):
    def letters(word):
        return tuple(word.count(letter) 
                     for letter 
                     in string.ascii_lowercase)
    return sum(map(min, letters(word1), letters(word2)))

if __name__ == '__main__':
    #keys = ['icily', 'strep', 'spork', 'spend', 'peeps', 'furls']

    soph  = {'icily': 0, 'strep': 2, 'spork': 1, 'spend': 2,
             'peeps': 2, 'furls': 1, 'ghost': 2, 'tanks': 2,
             'gecko': 2}

    jrs   = {'icily': 0, 'strep': 2, 'spork': 3, 'spend': 2,
             'peeps': 1, 'furls': 1, 'ghost': 1, 'tanks': 1,
             'gecko': 1}

    srs   = {'icily': 1, 'strep': 2, 'spork': 0, 'spend': 2,
             'peeps': 2, 'furls': 0, 'ghost': 1, 'tanks': 2,
             'gecko': 1}

    other = {'icily': 1, 'strep': 1, 'spork': 0, 'spend': 2,
             'peeps': 1, 'furls': 1, 'ghost': 0, 'tanks': 1,
             'gecko': 1, 'games': 1}

    #soph['tanks'] = 2

    classes = [soph, jrs, srs, other]

    with open('words.txt', 'r') as f:
        word_list = [line.strip() for line in f]

    def solve(exp_scores):
        def possible(word1):
            return all(score(word1, word2) == exp_score 
                       for word2, exp_score 
                       in exp_scores.iteritems())
        return filter(possible, word_list)

    for exp_scores in classes:
        print solve(exp_scores)

