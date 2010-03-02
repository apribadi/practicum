#!/usr/local/bin/python
#
# Script to test an ACM submission and see if it passes.  Should be
# run by the uid of the user doing the submission, not a privileged
# user (as it runs the submitted code, and would otherwise be a big
# security hole).
#
# Compiled files are placed in the current directory.
#
# Usage: /cs/ACM/acmSubmit <submission name>

import os.path as path
import re
import sys

from glob import glob
from itertools import ifilter, izip_longest
from string import Template
from subprocess import Popen, check_call, PIPE


# configuration

TEST_DIR = '/cs/ACM/Tests/Spring09'

# The language of a given file is determined by its extension.  The commands in
# compile are run in order.  The runcmd is run in a shell, taking input from
# stdin.
#
# The compile and runcmd strings are interpolated with $file (the path to the
# submitted file) and $name (the name of the problem) available.

language_config = [
    { 'ext':     '.c',
      'compile':  [ 'gcc $file -o $name -lm' ],
      'runcmd':  './$name'
    },
    { 'ext':     '.cs',
      'compile':  [ 'gmcs $file' ],
      'runcmd':  'mono $(name).exe'
    },
    { 'ext':     '.cc',
      'compile':  [ 'g++ $file -o $name' ],
      'runcmd': './$name'
    },
    { 'ext':     '.java',
      'compile':  [ 'javac -cp . $file' ],
      'runcmd':  'java -cp . $name'
    },
    { 'ext':     '.hs',
      'compile':  [ 'ghc -O --make $file -o $name' ],
      'runcmd':  './$name'
    },
    { 'ext':     '.py',
      'runcmd':  'python $file'
    },
    { 'ext':     '.pl',
      'runcmd':  'perl $file'
    },
    { 'ext':     '.php',
      'runcmd':  'php $file'
    },
    { 'ext':     '.ps',
      'runcmd':  'gs -dNODISPLAY -dNOPROMPT -q $file'
    },
    { 'ext':     '.rb',
      'runcmd':  'ruby $file'
    },
    { 'ext':     '.lua',
      'runcmd':  'lua $file'
    },
    { 'ext':     '.clj',
      'runcmd':  'java -cp clojure.jar:clojure-contrib.jar clojure.main $file'
    }
    ]

languages = dict((lang['ext'], lang) for lang in language_config)
    

# script

if __name__ == '__main__':
    print "acmSubmit (Version 2010)"

    problems = [path.basename(p) for p in glob(path.join(TEST_DIR, '*'))]

    def usage():
        print "Usage: /cs/ACM/acmSubmit <filename>"
        print "Where <filename> is one of:"
        for problem in problems:
            print problem

    if len(sys.argv) < 2:
        usage()
        sys.exit("No file given")

    file = sys.argv[1]
    name, ext = path.splitext(path.basename(file))

    if name not in problems:
        usage()
        sys.exit("Problem %s not found." % name)

    if ext not in languages:
        usage()
        sys.exit("File extension  %s not usable." % ext)


    # compilation, if any needed
    if 'compile' in languages[ext]:
        for cmd_t in languages[ext]['compile']:
            cmd = Template(cmd_t).substitute({'file': file, 'name': name})
            check_call(cmd)

    # test_files: index -> { in: file, out: file }
    test_files = {}

    for test_file in glob(path.join(TEST_DIR, name, '*')):
        match = re.search('([0-9]+)\.(\w+)\Z', test_file)
        if not match:
            continue
        index = int(match.group(1))
        suffix = match.group(2)
        if index not in test_files:
            test_files[index] = {}
        test_files[index][suffix] = test_file

    for index, files in test_files.items():
        if 'in' not in files or 'out' not in files:
            del test_files[index]

    # testing starts here
    print "Testing %s ..." % name
    print

    failed = False

    for index in sorted(test_files.keys()):
        if failed:
            break

        print "Running test %d ..." % index

        sub = Popen(
            '%s < %s' % (runcmd, test_files[index]['in']),
            shell=True,
            stdout=PIPE
            )
            
        answer_file = open(test_files[index]['out'], 'r')

        givens  = (line.strip() for line in sub.stdout if line.strip() != '')
        answers = (line.strip() for line in answer_file if line.strip() != '')

        for given, answer in izip_longest(givens, answers):
            if given == answer:
                continue

            failed = True
            print ""
            if given is None:
                print "Your submission's output was too short"
            elif answer is None:
                print "Your submission's output was too long"
            else 
                print "Your output line: %s" % given.strip()
                print "The solution line: %s" % solution.strip()
            break

        answer_file.close()

    if failed:
        print
        print "Your output differed from the expected output."
        print

    else:
        print
        print "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
        print "Congratulations! You've completed the %s problem." % name
        print "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
        print
