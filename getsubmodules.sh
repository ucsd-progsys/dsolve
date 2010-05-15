#!/bin/sh

if $1
then
  prefix="meatwad.ucsd.edu:/git/"
else
  prefix=$1"@meatwad.ucsd.edu:/git/"
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
