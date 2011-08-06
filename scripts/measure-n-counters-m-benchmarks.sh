#!/bin/bash

set -e

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

function usage() {
    echo "Usage: $0 REMOTE-HOST COUNTER-FILE BENCHMARK-FILE"
}

test $# -eq 3 || { usage; exit 1; }

function str_to_id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

#<remote host> <benchmark> <counters>
function go() {
    for try in $(seq 3); do
        echo "PERFORMING BENCHMARK '$2', try $try"
        echo do_measuring.sh -s "$(str_to_id $2)" "$1" "$3" "$2"
        if [ $? -eq 0 ]; then
            break
        fi
    done
}

CTRFILE="$2"
BENCHFILE="$3"
RHOST="$1"
MAX_CTRS=8
CTR_STRING=""

while read BENCHMARK; do
    CUR_CTRS=0
    while read CTR; do
        if [ -z "$CTR" ]; then
            echo "WARNING: empty counter"
            continue
        fi
        let CUR_CTRS=$CUR_CTRS+1

        if [ $CUR_CTRS -gt $MAX_CTRS ]; then
            go "$RHOST" "$BENCHMARK" "$CTR_STRING"
            CUR_CTRS=1
            CTR_STRING=""
        fi

        if [ -z "$CTR_STRING" ]; then
            CTR_STRING="$CTR"
        else
            CTR_STRING="$CTR_STRING,$CTR"
        fi
    done < "$CTRFILE"
    go "$RHOST" "$BENCHMARK" "$CTR_STRING"
done < "$BENCHFILE"
