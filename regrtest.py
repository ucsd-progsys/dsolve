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

import optparse, dsolve, socket, time
import external.misc.rtest as rtest

class Config (rtest.TestConfig):
  def __init__ (self, dargs, testdirs, logfile, threadcount):
    rtest.TestConfig.__init__ (self, testdirs, logfile, threadcount)
    if dargs != "":
      self.dargs = dargs.split(" ")
    else:
      self.dargs = list()

  def run_test (self, file):
    return dsolve.run(True, self.dargs + ["-bare", "-v", "0", "-no-simple", "-no-timing", file])

  def is_test (self, file):
    return file.endswith (".ml")

parser = optparse.OptionParser()
parser.add_option("-t", "--threads", dest="threadcount", default=1, type=int, help="spawn n threads")
parser.add_option("-o", "--opts", dest="opts", default="", type=str, help="additional arguments to dsolve")
parser.add_option("-p", "--posdir", dest="postests", default =[], action="append", help="postest directory")
parser.add_option("-n", "--negdir", dest="negtests", default=[], action="append", help="negtest directory")
options, args = parser.parse_args()
testdirs  = [(x, 0) for x in options.postests] 
testdirs += [(x, 1) for x in options.negtests]
testdirs += [("postests", 0), ("negtests", 1)] if testdirs == [] else []


now     = (time.asctime(time.localtime(time.time()))).replace(" ","_")
logfile = "testlogs/results_%s_%s" % (socket.gethostname (), now)
runner  = rtest.TestRunner (Config (options.opts, testdirs, logfile, options.threadcount))
runner.run ()
