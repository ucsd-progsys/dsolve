#!/usr/bin/python
# Copyright (c) 2008 The Regents of the University of California. All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
# IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
# OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
# TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

import common, sys, time
import itertools as it
import dsolve

testfiles = [("tests/rec/benchtests", 0)]

def runtest(filep, expected_status):
  file = filep[0]
  if file == "#": return (file, True)
  collect = int(filep[1])
  lqualifs = common.str_to_bool(filep[2])
  no_recrefs = common.str_to_bool(filep[3])
  no_simple = common.str_to_bool(filep[4])
  union = common.str_to_bool(filep[5])
  gflags = "-I tests"
  status = dsolve.gen_quals(file, True, lqualifs, collect, gflags)
  if status != 0: 
    print "Qualgen failed on %s" % file
    sys.exit(2)
  start = time.time()
  flags = ["-v", "0", "-I", "theories", "-no-timing"]
  if no_recrefs:
     flags += ["-no-recrefs"]
  if no_simple:
     flags += ["-no-simple"]
  if union:
     flags += ["-union-wfs"]
  status = dsolve.solve_quals(file, True, False, False, flags)
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
