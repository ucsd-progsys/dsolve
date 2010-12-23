SUBMODULES=`grep "path = " .gitmodules | sed "s/path =//"`

do_submodules () {
    local f=$1

    for SUBMODULE in $SUBMODULES; do
        $f $SUBMODULE || break
    done
}

check_branch () {
    cd $SUBMODULE
    git branch --contains HEAD | (grep "^* master$" -q || (echo "Submodule $SUBMODULE not on branch master!"; exit 1))
    IS_MASTER=$?
    cd - >> /dev/null
    [ $IS_MASTER = 0 ] || exit 1
}

check_submodule_pointer_at_master () {
    CURRENT=`git submodule status $SUBMODULE | cut -d " " -f 1 | tr -d '+'`
    MASTER=`cd $SUBMODULE; git log HEAD | head -1 | cut -d " " -f 2`
    [ $CURRENT = $MASTER ] || (echo "Submodule $SUBMODULE's current and master branches differ"; exit 1)
}

check_invariants () {
    (do_submodules check_submodule_pointer_at_master || exit 1)
    (do_submodules check_branch || exit 1)
}