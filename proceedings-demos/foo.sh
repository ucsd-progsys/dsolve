for j in `cat blagh`;
  do cd $j; for i in `ls *.old`;
    do i=${i%.old};
       cp ../../postests/${i/_/\/} ${i/\//_};
  done; cd ..;
done
  

