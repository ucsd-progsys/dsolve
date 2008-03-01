#!/bin/bash
#not actually a regression test
OCAML=./dsolve.py
FLAGS="-v 8"
POSFILE=postests
NEGFILE=negtests
#HANDQUALS_ONLY="yep"

if [[ $1 == "" ]]
then
  export TMPDIR=/tmp
else
  export TMPDIR=$1
fi

rm -f $TMPDIR/*.lilog*

echo RUNNING $POSFILE TESTS
for i in `cat $POSFILE`
do
  echo RUNNING: $i 
  printable_name=${i/\//_}
  LOGFILE=$TMPDIR"/"$printable_name".lilog"
  echo $i > $LOGFILE
  time $OCAML $FLAGS $i 1>> $LOGFILE 2>> $LOGFILE

  SUCC=$?

  #temporary fix for liquid.run's broken annotations
  mv -f $TMPDIR/liq.ml $TMPDIR/"$printable_name"
  mv -f $TMPDIR/liq.annot $TMPDIR/${printable_name%\.ml}".annot"

  #some stupid grep tricks to cut down on the output
  s=`grep -n "\#\#solve\#\#" $LOGFILE | tail -n1 -` 
  d=`grep -n "\#\#endtime\#\#" $LOGFILE | tail -n1 -`
  s=`expr "$s" : '\([0-9]*\)'`
  d=`expr "$d" : '\([0-9]*\)'`
  let "s = $d - $s - 1"
  let "d = $d - 1"
  f=`head -n$d $LOGFILE | tail -n$s -`
  echo $i > $LOGFILE.min
  echo "$f" >> $LOGFILE.min
  echo "

  " >> $LOGFILE.min

  
  if [ $SUCC = 0 ] 
  then 
    echo "SUCCESS!"
  else
    echo "FAILURE :("
  fi
  echo -n "Time in solver:  " 
  t=`grep "TOTAL" $LOGFILE | tail -n1`
  t=`echo "${t//TOTAL/}" | tr -ds " " " "`
  tt=`grep "Prover cache" $LOGFILE | tail -n1 | tr -s " " " "`
  ttt=`grep "matching" $LOGFILE | tail -n1 | tr -s " " " "`
  echo "$t"
  echo "$tt"
  echo "$ttt"
done
echo DONE

echo RUNNING NEGFILE
for i in `cat $NEGFILE`
do
  echo RUNNING: $i 
  time $OCAML $FLAGS $i &> $TMPDIR"/"${i/\//_}".lilog"
  if [ $? = 1 ] 
  then 
    echo "SUCCESS!"
  else
    echo "FAILURE :("
  fi
  grep "TOTAL" $LOGFILE | tail -n1
done
echo DONE

exit 0


