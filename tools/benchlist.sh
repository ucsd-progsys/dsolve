#!/bin/bash

FILE="/tmp/benchtree.ml"

cat - > $FILE <<EOF
type 'a lst = Nil | Cons of 'a * 'a lst

let l0 = Nil
EOF

CONSTRAINTS=`./dsolve.py -bare $FILE | grep "split subtyping constraints" | cut -d ' ' -f 1`
echo "0 ${CONSTRAINTS}"

for i in `seq 1 10`; do
  j=$((i - 1))
  cat - >> $FILE <<EOF
let l${i} = Cons(${i}, l${j})
EOF
  CONSTRAINTS=`./dsolve.py -bare $FILE | grep "split subtyping constraints" | cut -d ' ' -f 1`
  echo "${i} ${CONSTRAINTS}"
done
