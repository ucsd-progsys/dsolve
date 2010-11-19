#!/bin/sh

if $1
then
  prefix="goto.ucsd.edu:/home/git/"
else
  prefix=$1"@goto.ucsd.edu:/home/git/"
fi

z3=$prefix"external/z3"
fp=$prefix"external/fixpoint"
og=$prefix"external/ocamlgraph"
mi=$prefix"external/misc"

echo $z3
echo $fp
echo $og
echo $mi

cd external
git clone "$z3"
git clone "$fp"
git clone "$og"
git clone "$mi"
