#!/bin/bash

source "utils/libsubmodule.sh"

LFLG="-local"

checkout_master () {
    cd $1
    git checkout master
    cd - > /dev/null
}

git submodule init
if [ "$1" = "$LFLG" ]; then sed -e "s/goto\.ucsd\.edu\:/\/home/g" -i .git/config; fi
git submodule update
# To fix: we don't compile against master branches!
# do_submodules checkout_master
