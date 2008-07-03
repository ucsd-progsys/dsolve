#/bin/sh

cat $1 | tr "'" "@" | cpp -P - | tr "@" "'" > $2
