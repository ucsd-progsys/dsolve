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

import common, sys, time, os, os.path
import itertools as it
import dsolve

testdirs = [("postestsfix", 0)]

def runtest(file, expected_status):
  include = "theories"
  status = dsolve.gen_quals(file, False, "-I " + include)
  if status != 0: 
    print "Qualgen failed on %s" % file
    sys.exit(2)
  start = time.time()
  status = dsolve.solve_quals(file, True, False, True, ["-v", "0", "-fix", "-simpguard", "-no-simple", "-no-timing", "-I", include])
  #if status == 2: sys.exit(2)
  print "%f seconds" % (time.time() - start)

  ok = (status == expected_status)
  if ok:
    print "\033[1;32mSUCCESS!\033[1;0m\n"
  else:
    print "\033[1;31mFAILURE :(\033[1;0m\n"
  if ok:
    (_, _, f) = file.rpartition("/")
    os.system("mv /tmp/fix.in.fq %s/%s.in.fq" % (sys.argv[1], f))
    os.system("mv /tmp/fix.out.fq %s/%s.out.fq" % (sys.argv[1], f))
  return (file, ok)

def runtests(dir, expected_status):
  print "Running tests from %s/" % dir
  files = it.chain(*[[os.path.join(dir, file) for file in files] for dir, dirs, files in os.walk(dir)])
  return [runtest(file, expected_status) for file in files if file.endswith(".ml")]

results   = [runtests(dir, expected_status) for (dir, expected_status) in testdirs]
failed    = [result[0] for result in it.chain(*results) if result[1] == False]
failcount = len(failed)
if failcount == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;0m"
else:
  print "\n\033[1;31mFailed %d tests:\033[1;0m %s" % (failcount, ", ".join(failed))
sys.exit(failcount != 0)
