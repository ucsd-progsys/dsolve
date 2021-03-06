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

path   = os.path.dirname(sys.argv[0])
d_pats = "default_patterns"
solve  = "%s/liquid.opt" % (path)

ld_library_path = ""
if "LD_LIBRARY_PATH" in os.environ:
  ld_library_path = os.environ["LD_LIBRARY_PATH"]
os.environ["LD_LIBRARY_PATH"] = "%s/external/z3/lib/:%s" % (sys.path[0], ld_library_path)
del ld_library_path

def gen_quals(src,flags):
  bname = src[:-3]
  (fname,qname,hname) = (bname+".ml", bname+".quals", bname+".hquals")
  os.system("rm -f %s" % qname)
  files = []
  if os.path.exists(hname):
    files += [hname]
  if not "-bare" in flags:
    files += [d_pats]

  # TODO: Ensure that these files get deleted?
  (handle, tname) = tempfile.mkstemp()
  os.close(handle)
  (handle, tname2) = tempfile.mkstemp()
  os.close(handle)
  del handle

  common.cat_files(files, tname)
  gen  = ("%s %s -summarize" % (solve, flags)).split()
  succ = common.logged_sys_call(gen + [tname2, fname], False)
  if succ == 0:
    split= ("%s/depsplit %s %s %s" % (path, tname, tname2, qname)).split()
    common.logged_sys_call(split, True)
  return succ

def solve_quals(file,quiet,flags):
  bname = file[:-3]
  os.system("rm -f %s.annot" % bname)
  return common.logged_sys_call([solve, "-dframes"] + flags + [("%s.ml" % bname)], quiet)

def get_options(src):
  xs = [x.strip() for x in common.read_lines(src) if "(* DSOLVE" in x ]
  ss = ' '.join([' '] + [x[9:-2] for x in xs])
  xs = [x.strip() for x in ss.split(' ') if x.strip()]
  return xs

def run(quiet, flags):
  if len(flags) == 0:
    print ("Usage: %s [flags] [sourcefile]" % sys.argv[0])
    sys.exit(0)
  if "-help" in flags or "--help" in flags:
    os.system("%s -help" % (solve))
    return 0
  src   = flags[len(flags) - 1]
  flags = flags[:-1]
  flags.extend(get_options(src))
  gen_succ = gen_quals(src, " ".join(flags))
  if (gen_succ != 0):
    print "Qualifier generation failed"
    return gen_succ
  return solve_quals(src, quiet, flags)

if __name__ == "__main__":
  print "dsolve 0.1: Copyright (c) 2008-10 The Regents of the University of California, all rights reserved\n"
  sys.exit(run(False, sys.argv[1:]))
