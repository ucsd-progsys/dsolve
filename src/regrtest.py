#!/usr/bin/python

import common, sys, time
import itertools as it
import dsolve

testfiles = [("postests", 0), ("negtests", 1)]

def runtest(filep, expected_status):
  file = filep[0]
  collect = int(filep[1])
  status = dsolve.gen_quals(file, False, collect)
  if status != 0: 
    print "Qualgen failed on %s" % file
    sys.exit(2)
  start = time.time()
  status = dsolve.solve_quals(file, False, True, [" -v 0 "])
  if status == 2: sys.exit(2)
  print "%f seconds" % (time.time() - start)

  ok = (status == expected_status)
  if ok:
    print "\033[1;32mSUCCESS!\033[1;0m\n"
  else:
    print "\033[1;31mFAILURE :(\033[1;0m\n"
  return (file, ok)

def runtests(file, expected_status):
  print "Running tests from %s" % file
  return [runtest(test.rstrip().split(), expected_status) for test in common.read_lines(file)]

results   = [runtests(file, expected_status) for (file, expected_status) in testfiles]
failed    = [result[0] for result in it.chain(*results) if result[1] == False]
failcount = len(failed)
if failcount == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;0m"
else:
  print "\n\033[1;31mFailed %d tests:\033[1;0m %s" % (failcount, ", ".join(failed))
sys.exit(failcount != 0)
