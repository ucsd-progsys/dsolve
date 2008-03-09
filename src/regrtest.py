#!/usr/bin/python

import common
import sys
import time
import itertools as it

dsolve  = "./dsolve.py"
testfiles = [("postests", 0), ("negtests", 256)]

def runtest(file, expected_status):
  start = time.time()
  status = common.logged_sys_call("%s %s > /dev/null 2>&1" % (dsolve, file))
  print "%f seconds" % (time.time() - start)

  if status == expected_status:
    print "\033[1;32mSUCCESS!\033[1;37m\n"
    return (file, True)
  else:
    print "\033[1;31mFAILURE :(\033[1;37m\n"
    return (file, False)

def runtests(file, expected_status):
  print "Running tests from %s" % file
  return [runtest(test.rstrip(), expected_status) for test in common.read_lines(file)]

results = [runtests(file, expected_status) for (file, expected_status) in testfiles]
failed = [result[0] for result in it.chain(*results) if result[1] == False]
if len(failed) == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;37m"
else:
  print "\n\033[1;31mFailed %d tests:\033[1;37m %s" % (len(failed), ", ".join(failed))
sys.exit(len(failed) != 0)
