#!/usr/bin/python

import common, sys, time
import itertools as it

dsolve  = "./dsolve.py"
testfiles = [("postests", 0), ("negtests", 256)]

def runtest(file, expected_status):
  start = time.time()
  status = common.logged_sys_call("%s %s 1> /dev/null 2> /dev/null" % (dsolve, file))
  print "%f seconds" % (time.time() - start)

  ok = (status == expected_status)
  if ok:
    print "\033[1;32mSUCCESS!\033[1;0m\n"
  else:
    print "\033[1;31mFAILURE :(\033[1;0m\n"
  return (file, ok)

def runtests(file, expected_status):
  print "Running tests from %s" % file
  return [runtest(test.rstrip(), expected_status) for test in common.read_lines(file)]

results   = [runtests(file, expected_status) for (file, expected_status) in testfiles]
failed    = [result[0] for result in it.chain(*results) if result[1] == False]
failcount = len(failed)
if failcount == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;0m"
else:
  print "\n\033[1;31mFailed %d tests:\033[1;0m %s" % (failcount, ", ".join(failed))
sys.exit(failcount != 0)
