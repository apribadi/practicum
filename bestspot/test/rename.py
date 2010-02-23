# rename.py

import os
import sys

print sys.argv

if len(sys.argv) > 1:
  maxFileNum = int(sys.argv[1])
else:
  print "Usage:   python rename.py maxFileNum"
  sys.exit(0)

for i in range(1,maxFileNum+1):

  origFileName = "O." + str(i)
  newFileName = "test" + str(i) + ".out"

  print "origFileName is", origFileName
  print "newFileName is", newFileName

  os.rename( origFileName, newFileName )

  origFileName = "I." + str(i)
  newFileName = "test" + str(i) + ".in"

  print "origFileName is", origFileName
  print "newFileName is", newFileName

  os.rename( origFileName, newFileName )

