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

import common, sys, time, os, os.path, Queue, optparse, threading
import itertools as it
import external.misc.pmap as pmap
import dsolve

testdirs = [("postests", 0), ("negtests", 1)]

def runtest((file, expected_status)):
  start = time.time()
  status = dsolve.run(True, ["-bare", "-v", "0", "-no-simple", "-no-timing", file])
  print "%f seconds" % (time.time() - start)

  ok = (status == expected_status)
  if ok:
    print "\033[1;32mSUCCESS!\033[1;0m (%s)\n" % (file)
  else:
    print "\033[1;31mFAILURE :(\033[1;0m (%s) \n" % (file)
  return (file, ok)

class Worker(threading.Thread):
  def __init__(self, testqueue):
    threading.Thread.__init__(self)
    self.results   = list ()
    self.testqueue = testqueue

  def run(self):
    while not self.testqueue.empty():
      (file, expected_status) = self.testqueue.get()
      self.results.append(runtest(file, expected_status))
      self.testqueue.task_done()

def parseopts():
  parser = optparse.OptionParser()
  parser.add_option("-p", "--parallel", dest="threadcount", default=1, type=int, help="spawn n threads")
  options, args = parser.parse_args()
  return options

options = parseopts()

def dirtests(dir, expected_status):
  return it.chain(*[[(os.path.join(dir, file), expected_status) for file in files if file.endswith(".ml")] for dir, dirs, files in os.walk(dir)])

alltests  = it.chain(*[dirtests(dir, expected_status) for dir, expected_status in testdirs])
results   = pmap.map(options.threadcount, runtest, alltests)
failed    = [result[0] for result in results if result[1] == False]
failcount = len(failed)
if failcount == 0:
  print "\n\033[1;32mPassed all tests! :D\033[1;0m"
else:
  print "\n\033[1;31mFailed %d tests:\033[1;0m %s" % (failcount, ", ".join(failed))
sys.exit(failcount != 0)
