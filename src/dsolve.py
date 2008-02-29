#!/usr/bin/python

import sys
import os
import os.path

gen="./liquid.opt -no-anormal -dqualifs -lqualifs"
solve="./liquid.opt"

flags = ["-dframes"]

basename=sys.argv[len(sys.argv) - 1][:-3]
fname = basename + ".ml"
qname = basename + ".quals"
aname = basename + ".annot"

os.system("rm -f %s; rm -f %s" % (qname, aname))

if (sys.argv[1] != "--bare"):
  os.system("%s %s 1> /dev/null 2> %s" % (gen, fname, qname))
else:
  os.system("touch %s" % (qname))
  sys.argv.pop()

os.system("%s %s %s" % (solve, " ".join(flags + sys.argv[1:-1]), fname))

def fix_annotations():
  qualfile = open(qname)
  quallines = qualfile.readlines()
  qualfile.close()

  lineoffset = len(quallines)
  charoffset = len("".join(quallines))

  annotfile = open("/tmp/liq.annot")
  fixedannots = []

  for line in annotfile.readlines():
      fields = line.split(" ")
      if fields[0] == '"/tmp/liq.ml"':
          for i in [0, 4]:
              fields[i] = '"' + fname + '"'

          for i in [1, 5]:
              fields[i] = str(int(fields[i]) - lineoffset)

          for j in [2, 3, 6]:
              fields[j] = str(int(fields[j]) - charoffset)

          fields[7] = str(int(fields[7]) - charoffset)

      fixedannots.append(" ".join(fields))

  annotfile.close()
  return "\n".join(fixedannots)

fixedannots = fix_annotations()
annotfile = open(aname, "w")
annotfile.write(fixedannots)
annotfile.close()
