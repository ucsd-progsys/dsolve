#!/usr/bin/python

import sys
import os
import os.path

ocaml="./liquid.run"
qflags="-no-anormal -dqualifs -lqualifs"
flags="-dframes "
fname=sys.argv[len(sys.argv) - 1]

os.system("rm -f %s.quals; rm -f %s.annot" % (fname, fname))

if (sys.argv[1] != "--bare"):
  os.system("%s %s %s 1> /dev/null 2> %s.quals" % (ocaml, qflags, fname, fname))
else:
  os.system("touch %s.quals" % (fname))
  sys.argv.pop()

flags += " ".join(sys.argv[1:-1])
os.system("%s %s %s" % (ocaml, flags, fname))

base = os.path.basename(fname)
dir = os.path.dirname(fname)
os.system("cat /tmp/liq.annot | ./fix-annot.py %s > %s/%s.annot" % (fname, dir, base))
