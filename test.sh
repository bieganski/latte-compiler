#!/bin/bash

TESTS=`ls -1 lattests/bad/ | grep lat | cut -d \. -f 1`

MY_OUTS=`ls -1 lattests/bad/ | grep myout`

test_good() {
    echo "TESTING GOOD STARTING..."
    for t in $TESTS; do
        ./insc_llvm examples/$t.ins
        lli examples/$t.bc > examples/$t.myout
    done;
    compare_outs examples
    echo "TESTING GOOD DONE"
}

test_bad() {
    echo "TESTING BAD STARTING..."
    for t in $TESTS; do
        stack run lattests/bad/$t.lat
    done;
    compare_bad_outs lattests/bad
    echo "TESTING BAD DONE"

}

compare_bad_outs() {
    DIR=$1
    for t in $TESTS; do
        if grep -q ERROR ${DIR}/$t.myout; then
            echo "$t passed."
        else
            echo "$t NOT passsed!"
        fi
    done;
}


compare_outs() {
    DIR=$1
    for t in $TESTS; do
        if cmp -s ${DIR}/$t.myout examples/$t.output; then
            echo "$t passed."
        else
            echo "$t NOT passsed!"
        fi
    done;
}

clean() {
    for f in $MY_OUTS; do
        rm -rf $f
    done;
}

clean
test_bad
