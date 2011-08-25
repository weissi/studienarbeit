#!/bin/bash

set -e

trap err ERR
function err() {
    set +e
    echo "UNRECOVERABLE ERROR"
    if [ x$DATADUMPPID != x ]; then
        echo "INFO: Telling datadump ($DATADUMPPID) to stop"
        kill -INT $DATADUMPPID
    fi
    exit 1
}

function die() {
    set +e
    echo
    echo "ERROR: $@"
    if [ x$DATADUMPPID != x ]; then
        echo "INFO: Telling datadump ($DATADUMPPID) to stop"
        kill -INT $DATADUMPPID
    fi
    exit 1
}

function warn() {
    echo "WARNING: $@"
}

function remote() {
    set +e
    ssh -q "$RHOST" "$@"
    RET=$?
    set -e
    return $RET
}

function remotecp() {
    scp "$1" "${RHOST}:$2"
}

function str_to_id() {
    echo $* | tr -d -c a-zA-Z0-9_-
}

function usage() {
    echo -n "Usage $0 [-d] [-N] [-n] [-s SHOT-ID] [-p SHOT-ID-PREFIX] "
    echo -n "[-o OUT-DIR] [-b BENCHMARK-SCRIPT] REMOTE-HOST COUNTERS "
    echo "-b|BENCHMARK"
    echo
    echo "-n: no automatic tests and building"
    echo "-d: delete R table file after having calculated work"
    echo "-N: no measuring (dry-run)"
    echo
    echo "Defaults:"
    echo "  SHOT-ID-PREFIX: none"
    echo "  OUT-DIR: $HERE/measuring_data"
}

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
SHOTID="$(date +'%Y-%m-%d_%H-%M-%S')"
SHOT_ID_PREFIX=""
OUTDIR="$HERE/../measuring_data"
TEST_AND_BUILD=1
DO_MEASURING=1
DEL_RTAB=0
BENCHMARK_FILE=""

while getopts dNns:p:o:b: OPT; do
    case "$OPT" in
        p)
            SHOT_ID_PREFIX="$(str_to_id ${OPTARG})@"
            ;;
        s)
            SHOTID="$(str_to_id $OPTARG)"
            ;;
        o)
            OUTDIR="$OPTARG"
            ;;
        n)
            TEST_AND_BUILD=0
            ;;
        d)
            DEL_RTAB=1
            ;;
        b)
            BENCHMARK_FILE="$OPTARG"
            ;;
        N)
            DO_MEASURING=0
            ;;
        [?])
            usage
            exit 1
            ;;
    esac
done

OUTDIR=$(cd "$OUTDIR" && pwd)
if [ ! -z "$BENCHMARK_FILE" ]; then
    echo "$BENCHMARK_FILE"
    echo "$(dirname $BENCHMARK_FILE)"
    BENCHMARK_FILE=$(cd "$(dirname $BENCHMARK_FILE)" && pwd)/\
$(basename -- "$BENCHMARK_FILE")
fi

shift $(( $OPTIND-1 ))

if [ -z "$BENCHMARK_FILE" ]; then
    test $# -eq 3 || ( usage; exit 1; )
else
    echo "$BENCHMARK_FILE"
    test -f "$BENCHMARK_FILE" || ( echo "$BENCHMARK_FILE: not found"; exit 1; )
    test $# -eq 2 || ( usage; exit 1; )
fi

cd "$HERE/.."

RHOST="$1"
COUNTERS="$2"
RBENCH="$3"
LHOST="i30pc26"
LPORT="12345"
RSCRIPT="./__do_measuring_remote_script.sh"
RRUNSCRIPT="./__do_measuring_remote_run_benchmark"

SHOTID="${SHOT_ID_PREFIX}$SHOTID"

RPATH="studienarbeit/"
DPFILE="$OUTDIR/measured_${SHOTID}.dpts"
EXFILE="$OUTDIR/measured_${SHOTID}.rtab"
CTFILE="$OUTDIR/counters_${SHOTID}.ctrs"
CWFILE="$OUTDIR/work_${SHOTID}.work"
LOG="/tmp/measuring_log_$SHOTID.log"
REMLOG="/tmp/remote-${SHOTID}.log"

echo -n "No other 'datadump' running: "
! pgrep datadump 2> /dev/null || ( sleep 5; ! pgrep -l datadump ) \
    || die "datadump already running"
echo "OK"

if [ $DO_MEASURING -ne 0 ]; then
    echo -n "Checking if NI device 3923:7272 is plugged: "
    lsusb | grep -q '3923:7272' || die "NI USB-6218 not connected to USB"
    echo "OK"
fi

if [ $TEST_AND_BUILD -eq 1 ]; then
    echo -n "Testing password-free SSH: "
    START=$(date +%s)
    remote true
    let DIFF=1+$(date +%s)-$START
    test $DIFF -lt 3 || die "ssh not working correctly, took $DIFF s"
    echo "OK"

    echo -n "Testing password-free sudo: "
    START=$(date +%s)
    remote sudo true
    let DIFF=1+$(date +%s)-$START
    test $DIFF -lt 2 || die "sudo not working correctly, took $DIFF s"
    echo "OK"

    echo -n "Building: "
    ./build.sh &> /dev/null || die "building failed"
    echo "OK"

    echo -n "Remote building: "
    remote studienarbeit/build.sh &> /dev/null || die "remote building failed"
    echo "OK"
fi

if [ $DO_MEASURING -ne 0 ]; then
    echo "INFO: logfile is '$LOG'"
    datadump "$DPFILE" $SHOTID &> "$LOG" &
    DATADUMPPID=$!
    echo -n "Waiting for sloooow NI call (e.g. 29s) "
    START=$(date +%s)
    TMP=0
    while ! grep -q 'GOOOOOO!' "$LOG" 2> /dev/null; do
        let DIFF=1+$(date +%s)-$START
        sleep 0.2
        if [ $TMP -ne $DIFF ]; then
            echo -n "."
            TMP=$DIFF
        fi
        if [ $DIFF -gt 60 ]; then
            die "something went wrong, waited 60s and nothing happened"
        fi
    done
    let DIFF=1+$(date +%s)-$START
    test $DIFF -gt 10 || die "NI responding too fast ;-), only took $DIFF s"
    echo "OK ($DIFF s, pid=$DATADUMPPID)"

    sleep 1
fi

echo -n "Writing remote script: "
if [ -z "$BENCHMARK_FILE" ]; then
    remote tee "$RRUNSCRIPT" &> /dev/null <<EOF
#!/bin/bash
$RBENCH
EOF
else
    remotecp "$BENCHMARK_FILE" "$RRUNSCRIPT" > /dev/null
fi
remote chmod +x "$RRUNSCRIPT"

remote tee "$RSCRIPT" &> /dev/null <<EOF
#!/bin/bash
(
sleep 1
killall sshd
ps auxw
/home/weiss/studienarbeit/scripts/sudo_dumpcounters -s "$SHOTID" \
    -o "/tmp/$(basename $CTFILE)" -r "$RRUNSCRIPT" -e "$COUNTERS"
echo \$? | nc "$LHOST" -q 0 "$LPORT"
) < /dev/null &> "/tmp/$(basename $REMLOG)" &
EOF
echo OK

echo -n "Running remote benchmark: "
START=$(date +%s)
remote bash "$RSCRIPT"
RET=$(netcat -l -p "$LPORT")
let DIFF=$(date +%s)-$START
if [ "$RET" -eq "0" ]; then
    echo "OK (time=$DIFF)"
else
    warn "remote benchmark failed, see log '$REMLOG'"
    echo "done, but FAILURE (time=$DIFF, ret=$RET)"
fi
scp -q "$RHOST":"/tmp/$(basename $REMLOG)" "$REMLOG"
echo "INFO: remote log is '$REMLOG'"
scp -q "$RHOST":"/tmp/$(basename $CTFILE)" "$CTFILE"

if [ $DO_MEASURING -ne 0 ]; then
    if grep 'FINISHED!' "$LOG" &> /dev/null; then
        echo
        echo "WARNING: data measuring finished before benchmark+dump counter "\
"finished..."
        echo
    fi

    echo -n 'Telling datadump to stop (SIGINT): '
    kill -INT $DATADUMPPID
    echo "OK"

    TRIES=0
    echo -n "Waiting for dump process ($DATADUMPPID) to finish "
    while ps $DATADUMPPID &> /dev/null; do
        sleep 1
        echo -n .
        TRIES=$(($TRIES + 1))
        if [ $TRIES -gt 10 ]; then
            warn "datadump($DATADUMPPID) didn't exit after 10s, sending SIGINT"
            kill -INT $DATADUMPPID || true
        fi
        if [ $TRIES -gt 20 ]; then
            warn "datadump($DATADUMPPID) didn't exit after 20s, sending SIGTERM"
            kill $DATADUMPPID || true
        fi
        if [ $TRIES -gt 30 ]; then
            warn "datadump($DATADUMPPID) didn't exit after 30s, sending SIGKILL"
            kill -9 $DATADUMPPID || true
        fi
    done
    echo OK

    pgrep datadump | while read DDPID; do
        echo "WARNING: there is still a datadump running with pid $DDPID, "\
"killing it"
        set +e
        kill -INT $DDPID
        sleep 5
        kill $DDPID
        sleep 1
        kill -9 $DDPID
        set -e
    done

    echo -n "Exporting data to '$EXFILE' "
    dataexport "$DPFILE" > "$EXFILE"
    echo "OK"

    echo -n "Calculating work to '$CWFILE' "
    calculate_work.sh -s "$EXFILE" > "$CWFILE"
    echo "OK"

    if [ $DEL_RTAB -gt 0 ]; then
        echo -n "Deleting R table file '$EXFILE' "
        rm -- "$EXFILE"
        echo OK
    fi
fi

echo
echo "GREAT SUCCESS, EVERYTHING WENT FINE :-)"
