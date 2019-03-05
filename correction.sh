#!/bin/bash
for D in `find ./test/corr/ -type f`
do
    echo "The file :\n"
    cat $D
    if [ $# -eq 1 ] ; then
        lein run $D "-f" &
    else
        lein run $D &
    fi
    pid=$!
    echo "\n\nTesting the file:\n"
    wait $pid
    sleep 1
    echo "\n\n"
done
