#!/usr/bin/python

import sys, os, os.path, common

d_pats= "default_patterns"
gen   = "./liquid.opt -no-anormal -lqualifs -collect 4 -dqualifs"
solve = "./liquid.opt -dframes"
flags = []
tname = "/tmp/dsolve.scratch"

def cat_files(files,outfile):
  os.system("rm -f %s" % outfile)
  for f in files: os.system("cat %s 1>> %s 2> /dev/null" % (f,outfile))

def gen_quals(src,bare):
  (fname,qname,hname) = (src+".ml", src+".quals", src+".hquals")
  if bare:
    os.system("cp -f %s %s" % (hname, tname))
  else:
    cat_files([hname,d_pats],tname)
  common.logged_sys_call("%s %s %s 1> /dev/null 2> %s" % (gen, tname, fname, qname))

def solve_quals(src,flags):
  return common.logged_sys_call("%s %s %s.ml" % (solve, " ".join(flags),src))

def run(file,bare,flags):
  bname = file[:-3]
  os.system("rm -f %s.quals; rm -f %s.annot" % (bname, bname))
  gen_quals(bname,bare)
  return solve_quals(bname,flags)

def main():
  bare = (sys.argv[1] == "-bare")
  if bare: flags = sys.argv[2:-1]
  else: flags = sys.argv[1:-1]
  sys.exit(run(sys.argv[len(sys.argv) - 1],bare,flags))

if __name__ == "__main__":
  main()