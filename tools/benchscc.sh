#!/bin/bash

BASE="/tmp/benchscc"
FILE="${BASE}.ml"
HQUALS="${BASE}.hquals"

cat - > $HQUALS <<EOF
qualif EQ(v) : ~A { * * } v
qualif FLS(v): 1 = 0
EOF

for k in `seq 1 20`; do
    cat - > $FILE <<EOF
let rec makelist n =
  if n <= 0 then
    []
  else
    
EOF

    for i in `seq 1 $k`; do
        j=$((i - 1))
        echo -n "n :: " >> $FILE
    done
    echo -n "makelist (n - 1)" >> $FILE

    cat - >> $FILE <<EOF

let _ = makelist 20
EOF

    SUBITERS=`./dsolve.py $FILE | grep "Refine Iterations" | cut -d ',' -f 3 | cut -d '=' -f 2 | tr -d ')'`
    echo -e "${k}\t${SUBITERS}"
done