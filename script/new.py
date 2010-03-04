#!/usr/local/bin/python
#
# Script to test an ACM submission and see if it passes.  Should be
# run by the uid of the user doing the submission, not a privileged
# user (as it runs the submitted code, and would otherwise be a big
# security hole).
#
# Compilation, if any, is done in the current directory.
#
# Created by Aaron Pribadi, 3/2/2010
#
# Usage: /cs/ACM/acmSubmit <filename>

import collections
import datetime
import getpass
import glob
import os.path as path
import re
import subprocess
import sys
import time

from itertools import izip_longest
from string import Template


# configuration

TEST_DIR = '/cs/ACM/Tests/Spring09'

# The language of a given file is determined by its extension.  
# The commands in for compilation, if any, are run in order.
# The runcmd is run in a shell, taking input from stdin.
#
# The compile and runcmd strings are interpolated with $file (the path to the
# submitted file) and $problem (the name of the problem) available.

language_config = [
    { 'ext':      '.c',
      'language': 'C',
      'compile':  [ 'gcc $file -o $problem -lm' ],
      'runcmd':   './$problem'
    },
    { 'ext':      '.cs',
      'language': 'C#',
      'compile':  [ 'gmcs $file' ],
      'runcmd':   'mono $(problem).exe'
    },
    { 'ext':      '.cc',
      'language': 'C++',
      'compile':  [ 'g++ $file -o $problem' ],
      'runcmd':   './$problem'
    },
    { 'ext':      '.java',
      'language': 'Java',
      'compile':  [ 'javac -cp . $file' ],
      'runcmd':   'java -cp . $problem'
    },
    { 'ext':      '.hs',
      'language': 'Haskell',
      'compile':  [ 'ghc -O --make $file -o $problem' ],
      'runcmd':   './$problem'
    },
    { 'ext':      '.py',
      'language': 'Python',
      'runcmd':   'python $file'
    },
    { 'ext':      '.pl',
      'language': 'Perl',
      'runcmd':   'perl $file'
    },
    { 'ext':      '.php',
      'language': 'PHP',
      'runcmd':   'php $file'
    },
    { 'ext':      '.ps',
      'language': 'Postscript',
      'runcmd':   'gs -dNODISPLAY -dNOPROMPT -q $file'
    },
    { 'ext':      '.rb',
      'language': 'Ruby',
      'runcmd':   'ruby $file'
    },
    { 'ext':      '.lua',
      'language': 'Lua',
      'runcmd':   'lua $file'
    },
    { 'ext':      '.clj',
      'language': 'Clojure',
      'runcmd':   'java -cp clojure.jar:clojure-contrib.jar clojure.main $file'
    }
    ]

# the above language information, indexed by extension
languages = dict((lang['ext'], lang) for lang in language_config)
    
def usage():
    print "Usage: /cs/ACM/acmSubmit <filename>"


# script

if __name__ == '__main__':
    print "acmSubmit (Version 2010)"

    if len(sys.argv) < 2:
        usage()
        sys.exit()

    # problems: a list of available problems
    # file:     path to the submitted file
    # problem:  name of the submitted problem
    # ext:      file extension of the submission
    problems = [path.basename(p) for p in glob.glob(path.join(TEST_DIR, '*'))]
    file = sys.argv[1]
    problem, ext = path.splitext(path.basename(file))

    if problem not in problems:
        usage()
        print "\nProblem %s not found.  Should be one of:" % problem
        for available in problems:
            print available
        sys.exit()

    if ext not in languages:
        usage()
        print "\nFile extension  %s not accepted.  Should be one of:" % ext
        for available in languages.keys():
            print available
        sys.exit()

    lang_info = languages[ext]
    print "Language used: %s" % lang_info['language']

    # compilation, if any is needed
    if 'compile' in lang_info:
        for cmd_t in lang_info['compile']:
            cmd = Template(cmd_t).substitute({'file': file, 'problem': problem})
            subprocess.check_call(cmd, shell=True)

    # test_files: index -> { in: file, out: file }
    test_files = collections.defaultdict(dict)

    # grab all of the test files from the appropriate directory
    # extract the number of the test from the file name
    # extract the suffix of the test file (.in or .out)
    for test_file in glob.glob(path.join(TEST_DIR, problem, '*')):
        match = re.search('([0-9]+)\.(\w+)\Z', test_file)
        if not match:
            continue
        index, suffix = match.group(1, 2)
        test_files[int(index)][suffix] = test_file

    # make sure we have both input and output files
    for index, files in test_files.items():
        if 'in' not in files or 'out' not in files:
            del test_files[index]


    runcmd_t = lang_info['runcmd']
    runcmd = Template(runcmd_t).substitute({'file': file, 'problem': problem})

    # testing starts here
    print "\nTesting %s ...\n" % problem

    start_time = time.time()

    # keep track of whether we have failed a test
    failed = False

    for index, files in sorted(test_files.items()):
        if failed:
            break

        print "Running test %d ..." % index

        program = subprocess.Popen(
            '%s < %s' % (runcmd, files['in']),
            shell=True,
            stdout=subprocess.PIPE
            )
            
        answer_file = open(files['out'], 'r')

        output_answers = (line.strip() for line in program.stdout if line.strip() != '')
        correct_answers = (line.strip() for line in answer_file if line.strip() != '')

        for output, correct in izip_longest(output_answers, correct_answers):
            if output == correct:
                continue

            if output is None:
                print "\nYour submission's output was too short"
            elif correct is None:
                print "\nYour submission's output was too long"
            else:
                print "\nYour output line: %s" % output
                print "The solution line: %s" % correct

            failed = True
            break

        answer_file.close()

    if failed:
        print "\nFailure: Your output differed from the expected output.\n"
    else:
        elapsed_time = datetime.timedelta(seconds=time.time() - start_time)

        print
        print "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
        print "Congratulations! You've completed the %s problem." % problem
        print "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
        print 
        print "Elapsed time: %s" % elapsed_time
        print

        # mail to Dodds
        user = getpass.getuser()
        now = time.ctime()

        info = "%s (%s) %s" % (user, ext, problem)
        print '''mail -s "ACM at %s from %s (%s) %s in %s" dodds@cs.hmc.edu < %s''' % (now, user, ext, problem, elapsed_time, file)

       #subprocess.Popen(
       #    '''mail -s "ACM at %s from %s (%s) %s in %s" dodds@cs.hmc.edu < %s''' % (now, user, ext, problem, elapsed_time, file),
       #    shell=True
       #    )
