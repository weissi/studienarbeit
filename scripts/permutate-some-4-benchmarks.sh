#!/bin/bash

#calc in R
#pr <- function(n, r) ((factorial(n+r-1))/(factorial(r)*factorial(n-1)))
#pr(16, 4) = 4 CPUs, 16 benchmarks

set -e

trap unexpected_err ERR

function unexpected_err() {
    err "UNEXCEPTED ERROR"
    exit 1
}

function err() {
    echo >&2 "ERROR: $@"
}

function usage() {
    echo "Usage: $0 BENCHMARKS-FILE NUM-COMBINATIONS"
}

function s2id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

test $# -eq 2 || ( usage; exit 1; )
BENCHFILE="$1"
NUM="$2"

declare -a BENCHMARKS=( )
declare -a BMNS=( )
while read P_KEYWORD P_NAME LINE; do
    if [ "$P_KEYWORD" = "NAME" ]; then
        BMNS[${#BMNS[@]}]=$P_NAME
        BENCHMARKS[${#BENCHMARKS[@]}]=$LINE
    else
        err "please only use named benchmarks ($P_KEYWORD $P_NAME $LINE)"
        exit 1
    fi
done < "$BENCHFILE"
for (( i=0; i<$NUM; i++ )); do
    declare -a CONFIG=( )
    while read BMNUM; do
        CONFIG[${#CONFIG[@]}]=$BMNUM
    done < <(shuf -i 0-$((${#BENCHMARKS[@]} - 1)) -n4)
    test ${#CONFIG[@]} -eq 4
    NAME="$(s2id ${BMNS[CONFIG[0]]})_$(s2id ${BMNS[CONFIG[1]]})"
    NAME="${NAME}_$(s2id ${BMNS[CONFIG[2]]}_$(s2id ${BMNS[CONFIG[3]]}))"
    CMD1="taskset 1 ${BENCHMARKS[CONFIG[0]]}"
    CMD2="taskset 2 ${BENCHMARKS[CONFIG[1]]}"
    CMD3="taskset 4 ${BENCHMARKS[CONFIG[2]]}"
    CMD4="taskset 8 ${BENCHMARKS[CONFIG[3]]}"
    echo "NAME $NAME $CMD1 & $CMD2 & $CMD3 & $CMD4 & wait"
done
