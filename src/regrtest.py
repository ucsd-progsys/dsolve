#!/usr/bin/python

import common
import sys
import time

dsolve  = "./dsolve.py"
posfile = "postests"
negfile = "negtests"
tmpdir  = "/tmp"

def runtest(file, expected_status):
  start = time.time()
  status = common.logged_sys_call("%s %s > /dev/null 2>&1" % (dsolve, file))
  end = time.time()
  print "%f seconds" % (end - start)

  if status == expected_status:
    print "\033[1;32mSUCCESS!\033[1;37m\n"
    return (file, True)
  else:
    print "\033[1;31mFAILURE :(\033[1;37m\n"
    return (file, False)

def runtests(file, expected_status):
  print "Running %s" % file
  return [runtest(test.rstrip(), expected_status) for test in common.read_lines(file)]

(posresults, negresults) = (runtests(posfile, 0), runtests(negfile, 256))
allresults = posresults + negresults
failedtests = [result[0] for result in allresults if result[1] == False]
if len(failedtests) == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;37m"
  sys.exit(0)
else:
  print "\n\033[1;31mFailed %d tests:\033[1;37m %s" % (len(failedtests), ", ".join(failedtests))
  sys.exit(1)
