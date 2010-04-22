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

def gen_quals(src,flags):
  bname = src[:-3]
  (fname,qname,hname) = (bname+".ml", bname+".quals", bname+".hquals")
  os.system("rm -f %s" % qname)
  if "-bare" in flags and os.path.exists(hname):
    files = [hname]
  else:
    files = [hname, d_pats]
  common.cat_files(files, tname)
  flags += " " + " ".join(get_options(src))
  gen  = ("%s %s -summarize" % (solve, flags)).split()
  succ = common.logged_sys_call(gen + [tname2, fname])
  split= ("./depsplit %s %s %s" % (tname, tname2, qname)).split()
  common.logged_sys_call(split)
  return succ

def solve_quals(file,quiet,flags):
  bname = file[:-3]
  os.system("rm -f %s.annot" % bname)
  flags += get_options(file)
  if quiet: out = open("/dev/null", "w")
  else: out = None
  return common.logged_sys_call([solve, "-dframes"] + flags + [("%s.ml" % bname)], out)

def get_options(src):
  xs = [x.strip() for x in common.read_lines(src) if "(* DSOLVE" in x ]
  ss = ' '.join([' '] + [x[9:-2] for x in xs])
  xs = ['-'+ x.strip() for x in ss.split(' -') if x.strip()]
  return xs

def run(quiet, args):
  if len(args) == 1:
    print ("Usage: %s [flags] [sourcefile]" % sys.argv[0])
    sys.exit(0)
  if "-help" in sys.argv or "--help" in sys.argv:
    os.system("%s -help" % (solve))
    sys.exit(0)
  src      = args[len(args) - 1]
  flags    = args[1:-1]
  flags.extend(get_options(src))
  gen_succ = gen_quals(src, " ".join(flags))
  if (gen_succ != 0):
    print "Qualifier generation failed"
    return gen_succ
  return solve_quals(src, quiet, flags)

if __name__ == "__main__":
  print "dsolve 0.1: Copyright (c) 2008-10 The Regents of the University of California, all rights reserved\n"
  sys.exit(run(False, sys.argv))
