for j in `cat blagh`;
  do cd $j;
  for i in `ls *.annot`;
    do sed -i "s/proceedings-demos\/$j\///g" $i;
       i=${i%.annot};
       ./caml2html2 -ln -t -charset UTF-8 $i.ml;
       sed -i "s/$i\.ml/Hover mouse over variables to see types/g" $i.ml.html
       head -7 $i.ml.html > /tmp/blerg;
       cat  ../annot.css        >> /tmp/blerg;
       tail -n +8 $i.ml.html >> /tmp/blerg;
       cp /tmp/blerg $i.ml.html;
  done;
  cd ..;
done
