#!/bin/bash

set -e

trap err ERR
function err() {
    set +e
    echo "UNRECOVERABLE ERROR"
    if [ x$DATADUMPPID != x ]; then
        kill -INT $DATADUMPPID
    fi
    exit 1
}

function die() {
    set +e
    echo
    echo "ERROR: $@"
    if [ x$DATADUMPPID != x ]; then
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

function usage() {
    echo "Usage $0 [-n] [-s SHOT-ID] [-p SHOT-ID-PREFIX] [-o OUT-DIR] "\
"REMOTE-HOST COUNTERS BENCHMARK"
    echo
    echo "-n: no automatic tests and building"
    echo
    echo "Defaults:"
    echo "  SHOT-ID-PREFIX: none"
    echo "  OUT-DIR: $HERE/measuring_data"
}

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

SHOTID="$(date +'%Y-%m-%d_%H-%M-%S')"
SHOT_ID_PREFIX=""
OUTDIR="measuring_data"
TEST_AND_BUILD=1

while getopts ns:p:o: OPT; do
    case "$OPT" in
        p)
            SHOT_ID_PREFIX="${OPTARG}@"
            ;;
        s)
            SHOTID="$OPTARG"
            ;;
        o)
            OUTDIR="$OPTARG"
            ;;
        n)
            TEST_AND_BUILD=0
            ;;
        [?])
            usage
            exit 1
            ;;
    esac
done

shift $(( $OPTIND-1 ))

test $# -eq 3 || ( usage; exit 1; )

RHOST="$1"
COUNTERS="$2"
RBENCH="$3"
LHOST="i30pc26"
LPORT="12345"
RSCRIPT="__do_measuring_remote_script.sh"

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

echo -n "Checking if NI device 3923:7272 is plugged: "
lsusb | grep -q '3923:7272' || die "NI USB-6218 not connected to USB"
echo "OK"

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

echo "Hint: logfile is '$LOG'"
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

echo -n "Writing remote script: "
remote tee "$RSCRIPT" &> /dev/null <<EOF
#!/bin/bash
(
sleep 1
killall sshd
ps auxw
/home/weiss/studienarbeit/scripts/sudo_dumpcounters -s "$SHOTID" \
    -o "/tmp/$(basename $CTFILE)" -r "$RBENCH" -e "$COUNTERS"
echo \$? | nc "$LHOST" -q 0 "$LPORT"
) < /dev/null &> "/tmp/$(basename $REMLOG)" &
EOF
echo OK

echo -n "Running remote benchmark: "
START=$(date +%s)
remote bash "$RSCRIPT"
RET=$(netcat -l -p "$LPORT")
scp -q "$RHOST":"/tmp/$(basename $CTFILE)" "$CTFILE"
scp -q "$RHOST":"/tmp/$(basename $REMLOG)" "$REMLOG"
let DIFF=$(date +%s)-$START
if [ "$RET" -eq "0" ]; then
    echo "OK (time=$DIFF)"
else
    warn "remote benchmark failed, see log '$REMLOG'"
    echo "done, but FAILURE (time=$DIFF, ret=$RET)"
fi

if grep 'FINISHED!' "$LOG" &> /dev/null; then
    echo
    echo "WARNING: data measuring finished before benchmark+dump counter "\
"finished..."
    echo
fi

echo -n 'Telling datadump to stop (SIGINT): '
kill -INT $DATADUMPPID
echo "OK"

echo -n "Waiting for dump process ($DATADUMPPID) to finish "
while ps $DATADUMPPID &> /dev/null; do
    sleep 1
    echo -n .
done
echo OK

pgrep datadump | while read DDPID; do
    echo "WARNING: there is still a datadump running with pid $DDPID, killing it"
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

echo
echo "GREAT SUCCESS, EVERYTHING WENT FINE :-)"
