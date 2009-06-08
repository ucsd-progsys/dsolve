for j in `cat blarg`;
  do for i in `cat dont_delete_me`;
    do cp ../../postests/$i.$j run/${i//\//_}.$j;
  done;
done
  

