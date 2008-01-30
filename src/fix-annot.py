#!/usr/bin/python

import sys

qualfile = open(sys.argv[1] + ".quals")
quallines = qualfile.readlines()
lineoffset = len(quallines)
charoffset = len("".join(quallines))
print charoffset
print quallines

for line in sys.stdin.readlines():
    fields = line.split(" ")
    if fields[0] == '"/tmp/liq.ml"':
        for i in [0, 4]:
            fields[i] = '"' + sys.argv[1] + '"'

        for i in [1, 5]:
            fields[i] = str(int(fields[i]) - lineoffset)

        for j in [2, 3, 6]:
            fields[j] = str(int(fields[j]) - charoffset)

        fields[7] = str(int(fields[7]) - charoffset) + "\n"

    print " ".join(fields),
