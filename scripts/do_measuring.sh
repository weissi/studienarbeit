#!/bin/bash

set -e

trap err ERR
function err() {
    echo "UNRECOVERABLE ERROR"
    exit 1
}

function die() {
    echo
    echo "ERROR: $@"
    exit 1
}

function remote() {
    ssh -qt "$RHOST" "$@"
}

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

test $# -eq 3 || ( echo "Usage $0 REMOTE-HOST RUNNING-TIME BENCHMARK"; exit 1; )

RHOST="$1"
RUNTIME="$2"
RBENCH="$3"
SHOTID=$(date +"%Y-%m-%d_%H-%M-%S")
let DUMPRUNTIME=$RUNTIME+10

RPATH="studienarbeit/"
DPFILE="measured_${SHOTID}.dpts"
EXFILE="measured_${SHOTID}.rtab"
CTFILE="counters_${SHOTID}.ctrs"
LOG="/tmp/measuring_log_$SHOTID.log"
CTLOG="/tmp/counter-dump_${SHOTID}.log"
REMLOG="/tmp/remote-${SHOTID}.log"

echo -n "No other 'datadump' running: "
! pgrep datadump || die "datadump already running"
echo "OK"

echo -n "Checking if NI device 3923:7272 is plugged: "
lsusb | grep -q '3923:7272' || die "NI USB-6218 not connected to USB"
echo "OK"

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

echo "Hint: logfile is '$LOG'"
datadump "$DPFILE" $SHOTID $DUMPRUNTIME &> "$LOG" &
DATADUMPPID=$!
echo -n "Waiting for sloooow NI call (e.g. 29s) "
START=$(date +%s)
TMP=0
while ! grep -q 'GOOOOOO!' "$LOG"; do
    let DIFF=1+$(date +%s)-$START
    sleep 0.2
    if [ $TMP -ne $DIFF ]; then
        echo -n "."
        TMP=$DIFF
    fi
    if [ $DIFF -gt 35 ]; then
        die "something went wrong, waited 35s and nothing happened"
    fi
done
let DIFF=1+$(date +%s)-$START
test $DIFF -gt 10 || die "NI responding too fast ;-), only took $DIFF s"
echo "OK ($DIFF s, pid=$DATADUMPPID)"

sleep 2

echo -n "Running dumpcounters in background: "
remote /home/weiss/studienarbeit/scripts/sudo_dumpcounters -s "$SHOTID" \
    -o - -d $RUNTIME 2> "$CTLOG" > "$CTFILE" &
DC_PID=$!
echo "OK (pid=$DC_PID)"

echo -n "Running remote benchmark (remote log: '$REMLOG'): "
START=$(date +%s)
remote $RBENCH &> "$REMLOG"
let DIFF=$(date +%s)-$START
echo "OK ($DIFF s)"

echo -n "Waiting for dump counter process ($DC_PID) to finish "
while ps $DC_PID &> /dev/null; do
    sleep 1
    echo -n .
done
echo "OK"

if grep 'FINISHED!' "$LOG" &> /dev/null; then
    echo
    echo "WARNING: data measuring finished before benchmark+dump counter "\
"finished..."
    echo
fi

echo -n "Waiting for dump process ($DATADUMPPID) to finish "
while ps $DATADUMPPID &> /dev/null; do
    sleep 1
    echo -n .
done
echo OK

echo -n "Exporting data to '$EXFILE' "
dataexport "$DPFILE" > "$EXFILE"
echo "OK"
