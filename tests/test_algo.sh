#!/usr/bin/env bash
while true 
do
    bin/randomgame $1 $1 2 6 > tests/failtest_script.gm
    bin/pgsolver -global succsmallpm -ve tests/failtest_script.gm
    retval=$?
    if [ $retval -ne 0 ]; then
        echo "Error found. Game that caused it saved in tests/failtest_script.gm"
        exit 0
    fi
done
