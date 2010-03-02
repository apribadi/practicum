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

import contextlib
import os.path as path
import re
import sys

from glob import glob
from itertools import ifilter, izip_longest
from subprocess import Popen, check_call, PIPE


# configuration
test_dir = "/cs/ACM/Tests/Spring09"

def usage():
    print "Usage: /cs/ACM/acmSubmit <filename>"
    print "Where <filename> is one of:"
    for problem in sorted(glob(test_dir + "/*")):
        print problem

# languages
languages = {}

def handles(suffix):  
    def set_as_handler(func):
        languages[suffix] = func
        return func
    return set_as_handler

@handles(".c")
def c_lang(file, name, ext):
    check_call("gcc %(file)s -o %(name)s -lm" % locals(), shell=True)
    return "./%(name)s" % locals()

@handles(".cs")
def c_sharp(file, name, ext):
    check_call("gmcs %(file)s" % locals(), shell=True)
    return "mono ./%(name)s.exe" % locals()

@handles(".cc")
def cplusplus(file, name, ext):
    check_call("g++ %(file)s -o %(name)s" % locals(), shell=True)
    return "./%(name)s" % locals()

@handles(".java")
def java(file, name, ext):
    check_call("javac -cp . %(file)s" % locals(), shell=True)
    return "java -cp . %(name)s" % locals()

@handles(".hs")
def java(file, name, ext):
    check_call("ghc -O --make %(file)s -o %(name)s" % locals(), shell=True)
    return "./%(name)s" % locals()

@handles(".py")
def python(file, name, ext):
    return "python %(file)s" % locals()

@handles(".pl")
def perl(file, name, ext):
    return "perl %(file)s" % locals()

@handles(".php")
def php(file, name, ext):
    return "php ./%s" % locals()

@handles(".ps")
def postscript(file, name, ext):
    return "gs -dNODISPLAY -dNOPROMPT -q ./%(file)s" % locals()

@handles(".rb")
def ruby(file, name, ext):
    return "ruby %(file)s" % locals()

@handles(".lua")
def lua(file, name, ext):
    return "lua ./%(file)s" % locals()

@handles(".clj")
def clojure(file, name, ext):
	dir = "/home/dodds/public_html/ACM"
	classpath = "%(dir)s/clojure.jar:%(dir)s/clojure-contrib.jar" % locals()
	return "java -cp %(classpath)s clojure.main %(file)s" % locals()

if __name__ == "__main__":
    print "acmSubmit (Version 2010)\n"

    problems = [path.basename(p) for p in glob(path.join(test_dir, "*"))]

    if len(sys.argv) < 2:
        usage()
        sys.exit("No file given")

    file = sys.argv[1]
    name, ext = path.splitext(path.basename(file))

    if name not in problems:
        usage()
        sys.exit("File name not valid")

    if ext not in languages:
        usage()
        sys.exit("File extension not valid")

    # language-specific compilation, etc.
    # runcmd is a string that may be run in a shell
    # and accepts input from stdin
    runcmd = languages[ext](file, name, ext)

    # test_files: index -> { in: file, out: file }
    test_files = {}

    for test_file in glob(path.join(test_dir, name, "*")):
        match = re.search("([0-9]+)\.(\w+)\Z", test_file)
        if match:
            index = int(match.group(1))
            suffix = match.group(2)
            test_files.setdefault(index, {})[suffix] = test_file

    test_files = dict(kv for kv in test_files.iteritems()
                         if 'in' in kv[1] and 'out' in kv[1])

    # testing starts here!
    print "Testing %s ..." % name
    print

    failed = False

    for index in sorted(test_files.iterkeys()):
        print "Running test %d ..." % index

        input_file = test_files[index]["in"]
        output_file = test_files[index]["out"]

        sub = Popen(
            "%s < %s" % (runcmd, input_file),
            shell=True,
            stdout=PIPE
            )
            
        f = open(output_file, "r")

        givens = ifilter(lambda s: s.strip() != "", sub.stdout)
        solutions = ifilter(lambda s: s.strip() != "", f)

        for given, solution in izip_longest(givens, solutions):
            if given is None:
                failed = True
		print
                print "Your submission's output was too short"
                break
            if given != solution:
                failed = True
		print
                print "Your output line: %s" % given.strip()
                print "The solution line: %s" % solution.strip()
		break

        sub.terminate()
        f.close()

        if failed:
            break

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
