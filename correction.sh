#!/bin/bash
for D in `find ./test/corr/ -type f`
do
    echo -e "The file :\n"
    cat $D
    if [ $# -eq 1 ] ; then
        lein run $D "-f" &
    else
        lein run $D &
    fi
    pid=$!
    echo -e "\n\nTesting the file:\n"
    wait $pid
    sleep 10
    echo -e "\n\n"
done
