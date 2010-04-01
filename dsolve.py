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

import sys, os, os.path, common, tempfile

d_pats= "default_patterns"
solve = "./liquid.opt"

flags = []
null  = open("/dev/null", "w")
ld_library_path = ""
if "LD_LIBRARY_PATH" in os.environ:
  ld_library_path = os.environ["LD_LIBRARY_PATH"]
os.environ["LD_LIBRARY_PATH"] = "%s/external/z3/lib/:%s" % (sys.path[0], ld_library_path)
del ld_library_path

# TODO: Ensure that these files get deleted?
(handle, tname) = tempfile.mkstemp()
os.close(handle)
(handle, tname2) = tempfile.mkstemp()
os.close(handle)
del handle

def get_options(src):
  xs = [x.strip() for x in common.read_lines(src) if "(* DSOLVE" in x ]
  ss = ' '.join([' '] + [x[9:-2] for x in xs])
  xs = ['-'+ x.strip() for x in ss.split(' -') if x.strip()]
  return xs

def gen_quals(src,bare,flags):
  bname = src[:-3]
  (fname,qname,hname) = (bname+".ml", bname+".quals", bname+".hquals")
  os.system("rm -f %s" % qname)
  if bare and os.path.exists(hname):
    files = [hname]
  else:
    files = [hname, d_pats]
  common.cat_files(files, tname)
  gen  = ("%s %s -summarize" % (solve, flags)).split()
  succ = common.logged_sys_call(gen + [tname2, fname])
  split= ("./depsplit %s %s %s" % (tname, tname2, qname)).split()
  common.logged_sys_call(split)
  return succ

def solve_quals(file,bare,time,quiet,flags):
  bname = file[:-3]
  os.system("rm -f %s.annot" % bname)
  if quiet: out = null
  else: out = None
  if time: time = ["time"]
  else: time = []
  return common.logged_sys_call(time + [solve, "-dframes"] + flags + [("%s.ml" % bname)], out)

def main():
  argc = len(sys.argv)
  if argc == 1:
    print ("Usage: %s [flags] [sourcefile]" % sys.argv[0])
    sys.exit(0)
  if sys.argv[1] == "-help" or sys.argv[1] == "--help":
    os.system("%s -help" % (solve))
    sys.exit(0)
  bare = (sys.argv[1] == "-bare")
  if bare:
    sys.argv.pop(1)
  include = (sys.argv[1] == "-I")
  if include:
    include = sys.argv[1] + " " + sys.argv[2]
  else:
    include = ""
  time = (sys.argv[1] == "-time")
  if time:
    sys.argv.pop(1)
  src      = sys.argv[len(sys.argv) - 1]
  flags    = sys.argv[1:-1] 
  flags   += get_options(src)
  if "-dontgenmlq" in flags:
    include += " -dontgenmlq" 
  gen_succ = gen_quals(src, bare, include)
  if (gen_succ != 0):
    print "Qualifier generation failed"
    sys.exit(gen_succ)
  sys.exit(solve_quals(src, bare, time, False, flags))

if __name__ == "__main__":
  print "dsolve 0.1: Copyright (c) 2008-10 The Regents of the University of California, all rights reserved\n"
  main()
