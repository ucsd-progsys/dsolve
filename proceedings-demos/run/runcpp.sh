#/bin/sh

OUTPUT=`echo $1 | sed 's/\\.cpp//g'`
cat $1 | tr "'" "@" | cpp -P - | tr "@" "'" > $OUTPUT
