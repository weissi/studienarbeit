#!/bin/bash

trap err ERR
function err() {
    echo >&2 "UNRECOVERABLE ERROR"
    exit 2
}

set -e

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

function usage() {
    echo "Usage: $0 REMOTE-HOST OUT-DIR COUNTER-FILE BENCHMARK-FILE"
}

test $# -eq 4 || { usage; exit 1; }

RHOST="$1"
OUTDIR="$2"
CTRFILE="$3"
BENCHFILE="$4"

function str_to_id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

#<remote host> <output dir> <benchmark> <counters>
function go() {
    local RHOST="$1"
    local OUTDIR="$2"
    local BM="$3"
    local CTRS="$4"

    if [ $(echo "$CTRS" | grep -o , | wc -l) -gt $MAX_CTRS ]; then
        echo "WARNING: Having more than $MAX_CTRS counters: $CTRS"
        exit 1
    fi

    set +e
    let RUNS=$RUNS+1
    for try in $(seq 3); do
        set +e
        echo
        echo "--------------------"
        echo "PERFORMING BENCHMARK $RUNS of $TOTAL_RUNS: '$BM', try $try"
        echo "COUNTERS: '$CTRS'"
        echo
        pgrep datadump | while read DDPID; do
            echo "WARNING: datadump ($DDPID) running, killing"
            kill -INT $DDPID
            sleep 5
            kill $DDPID
            sleep 1
            kill -9 $DDPID
        done
        echo
        T_START=$(date +%s)
        do_measuring.sh -d -n -p "$(str_to_id $BM)" -o "$OUTDIR" "$RHOST" "$CTRS" "$BM"
        RET=$?
        let DIFF=$(date +%s)-$T_START || true
        echo "BENCHMARK DONE: return code=$RET, time=${DIFF}s"
        if [ $RET -eq 0 ]; then
            break
        fi
        set -e
    done
    if [ $RET -ne 0 ]; then
        echo "ERROR: BENCHMARK '$BM' FAILED AFTER 3 TRIES, GIVING UP..."
    fi
    set -e
    return 0
}

if [ ! -d "$OUTDIR" ]; then
    mkdir -p -- "$OUTDIR"
fi

MAX_CTRS=8
RUNS=0
CTR_STRING=""

declare -a BENCHMARKS=( )
while read LINE; do
    BENCHMARKS[${#BENCHMARKS[@]}]=$LINE
done < "$BENCHFILE"
COUNTERS=( $( cat -- "$CTRFILE" ) )
TOTAL_RUNS=$(( ${#BENCHMARKS[@]} * ${#COUNTERS[@]} / $MAX_CTRS ))

for BENCHMARK in "${BENCHMARKS[@]}"; do
    CTR_STRING=""
    CUR_CTRS=0
    for CTR in "${COUNTERS[@]}"; do
        if [ -z "$CTR" ]; then
            echo "WARNING: empty counter"
            continue
        fi
        let CUR_CTRS=$CUR_CTRS+1 || true

        if [ $CUR_CTRS -gt $MAX_CTRS ]; then
            go "$RHOST" "$OUTDIR" "$BENCHMARK" "$CTR_STRING"
            CUR_CTRS=1
            CTR_STRING=""
        fi

        if [ -z "$CTR_STRING" ]; then
            CTR_STRING="$CTR"
        else
            CTR_STRING="$CTR_STRING,$CTR"
        fi
    done
    echo "counter file '$CTRFILE' done after $RUNS runs"
    go "$RHOST" "$OUTDIR" "$BENCHMARK" "$CTR_STRING"
done
echo "benchmark file '$CTRFILE' done after $RUNS runs"
exit 0
