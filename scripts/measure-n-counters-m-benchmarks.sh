#!/bin/bash

trap err ERR
function err() {
    echo >&2 "UNRECOVERABLE ERROR"
    exit 2
}

set -e

WARM_UP_TMP="/tmp/WARM-UP-TMP"
DRY_RUN=0

function usage() {
    echo "Usage: $0 [-C MAX] REMOTE-HOST OUT-DIR COUNTER-FILE BENCHMARK-FILE"
    echo
    echo "-C: maximal number of simultaneous counters, default: 8"
}

MAX_CTRS=8
if [ "$1" = "-n" ]; then
    DRY_RUN=1
    shift
fi
if [ "$1" = "-C" ]; then
    MAX_CTRS="$2"
    shift; shift
fi

test $# -eq 4 || { usage; exit 1; }

RHOST="$1"
OUTDIR="$2"
CTRFILE=$(cd $(dirname -- "$3") && pwd)/$(basename -- "$3")
BENCHFILE=$(cd $(dirname -- "$4") && pwd)/$(basename -- "$4")

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

function str_to_id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

#<remote host> <output dir> <benchmark> <counters>
function go() {
    WARMUP_OPTS=""
    if [ "$1" = "-w" ]; then
        WARMUP_OPTS="-N"
        shift
    fi
    test $# -eq 5
    local RHOST="$1"
    local OUTDIR="$2"
    local BM="$3"
    local BMNAME="$4"
    local CTRS="$5"

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
        echo "PERFORMING BENCHMARK $RUNS of $TOTAL_RUNS: $BMNAME, try $try"
        echo "CMD: $BM"
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
        if [ $DRY_RUN -ne 1 ]; then
            do_measuring.sh $WARMUP_OPTS -f -d -n -p "$BMNAME" -o "$OUTDIR" \
                -t bzip2 "$RHOST" "$CTRS" "$BM"
            RET=$?
        else
            echo "DRY RUN"
            echo "  PREFIX: '$BMNAME'"
            echo "  OUTDIR: '$OUTDIR'"
            echo "  RHOST: '$RHOST'"
            echo "  CTRS: '$CTRS'"
            echo "  CMD: '$BM'"
            echo "  WARMUP_OPTS: '$WARMUP_OPTS'"
            RET=0
        fi
        let DIFF=$(date +%s)-$T_START || true
        echo "BENCHMARK DONE: return code=$RET, time=${DIFF}s"
        if [ $RET -eq 0 ]; then
            break
        fi
        set -e
    done
    if [ $RET -ne 0 ]; then
        echo "ERROR: BENCHMARK '$BMNAME' FAILED AFTER 3 TRIES, GIVING UP..."
    fi
    set -e
    return 0
}

if [ ! -d "$OUTDIR" ]; then
    mkdir -p -- "$OUTDIR"
fi

RUNS=0
CTR_STRING=""

declare -a BENCHMARKS=( )
declare -a BENCHMARK_NAMES=( )
while read P_KEYWORD P_NAME LINE; do
    if [ "$P_KEYWORD" = "NAME" ]; then
        BENCHMARK_NAMES[${#BENCHMARK_NAMES[@]}]=$P_NAME
        BENCHMARKS[${#BENCHMARKS[@]}]=$LINE
    else
        LINE="${P_KEYWORD} ${P_NAME} ${LINE}"
        BENCHMARK_NAMES[${#BENCHMARK_NAMES[@]}]=$(str_to_id "$LINE")
        BENCHMARKS[${#BENCHMARKS[@]}]=$LINE
    fi
done < "$BENCHFILE"
COUNTERS=( $( cat -- "$CTRFILE" ) )
TOTAL_RUNS=$(python -c "import math; print ${#BENCHMARKS[@]} * "\
"    int(math.ceil(float(${#COUNTERS[@]}) / $MAX_CTRS) + 1)")

for (( i=0; i<${#BENCHMARKS[@]}; i++ )); do
    BENCHMARK="${BENCHMARKS[$i]}"
    BENCHMARK_NAME="${BENCHMARK_NAMES[$i]}"
    CTR_STRING=""
    CUR_CTRS=0
    echo "INFO: warming up benchmark..."
    if [ ! -d "$WARM_UP_TMP" ]; then
        mkdir -p -- "$WARM_UP_TMP"
    fi
    go -w "$RHOST" "$WARM_UP_TMP" "$BENCHMARK" "warmup-for-$BENCHMARK_NAME" \
        "UOPS_ISSUED"
    for CTR in "${COUNTERS[@]}"; do
        if [ -z "$CTR" ]; then
            echo "WARNING: empty counter definition"
            continue
        fi
        let CUR_CTRS=$CUR_CTRS+1 || true

        if [ $CUR_CTRS -gt $MAX_CTRS ]; then
            go "$RHOST" "$OUTDIR" "$BENCHMARK" "$BENCHMARK_NAME" "$CTR_STRING"
            CUR_CTRS=1
            CTR_STRING=""
        fi

        if [ -z "$CTR_STRING" ]; then
            CTR_STRING="$CTR"
        else
            CTR_STRING="$CTR_STRING,$CTR"
        fi
    done
    go "$RHOST" "$OUTDIR" "$BENCHMARK" "$BENCHMARK_NAME" "$CTR_STRING"
done
exit 0
