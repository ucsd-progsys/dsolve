#!/usr/bin/python

import sys, os, os.path, common

d_pats= "default_patterns"
gen   = "./liquid.opt -no-anormal -lqualifs -collect 4 -dqualifs".split()
solve = "./liquid.opt -dframes".split()
flags = []
tname = "/tmp/dsolve.scratch"
null  = open("/dev/null", "w")

def cat_files(files,outfile):
  os.system("rm -f %s" % outfile)
  for f in files: os.system("cat %s 1>> %s 2> /dev/null" % (f,outfile))

def gen_quals(src,bare):
  (fname,qname,hname) = (src+".ml", src+".quals", src+".hquals")
  if bare:
    os.system("cp -f %s %s" % (hname, tname))
  else:
    cat_files([hname,d_pats],tname)
  qfile = open(qname, "w")
  common.logged_sys_call(gen + [tname, fname], null, qfile)
  qfile.close()

def solve_quals(file,bare,quiet,flags):
  bname = file[:-3]
  os.system("rm -f %s.quals; rm -f %s.annot" % (bname, bname))
  gen_quals(bname,bare)
  if quiet: out = null
  else: out = None
  return common.logged_sys_call(solve + flags + [("%s.ml" % bname)], out)

def main():
  bare = (sys.argv[1] == "-bare")
  if bare: flags = sys.argv[2:-1]
  else: flags = sys.argv[1:-1]
  sys.exit(solve_quals(sys.argv[len(sys.argv) - 1],bare,False,flags))

if __name__ == "__main__":
  main()
