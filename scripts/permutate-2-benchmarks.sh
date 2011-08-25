#!/bin/bash

set -e

trap unexpected_err ERR

function unexpected_err() {
    err "unexpected"
    exit 1
}

function err() {
    echo >&2 "ERROR: $@"
}

function usage() {
    echo "Usage: $0 BENCHMARKS-FILE"
}

function str_to_id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

test $# -eq 1 || ( usage; exit 1; )
BENCHFILE="$1"

declare -a BENCHMARKS=( )
declare -a BM_NAMES=( )
while read P_KEYWORD P_NAME LINE; do
    if [ "$P_KEYWORD" = "NAME" ]; then
        BM_NAMES[${#BM_NAMES[@]}]=$P_NAME
        BENCHMARKS[${#BENCHMARKS[@]}]=$LINE
    else
        err "please only use named benchmarks ($P_KEYWORD $P_NAME $LINE)"
        exit 1
    fi
done < "$BENCHFILE"

for (( i=0; i<${#BENCHMARKS[@]}; i++ )); do
    for (( j=$i; j<${#BENCHMARKS[@]}; j++ )); do
        NAME="$(str_to_id ${BM_NAMES[$i]})_$(str_to_id ${BM_NAMES[$j]})"
        CMD1="taskset 1 ${BENCHMARKS[$i]}"
        CMD2="taskset 2 ${BENCHMARKS[$j]}"
        echo "NAME $NAME $CMD1 & $CMD2 & wait"
    done
done
