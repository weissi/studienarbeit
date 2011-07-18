#!/bin/bash

set -e
trap err ERR
function err() {
    echo >&2 "UNRECOVERABLE ERROR"
    if [ -f "$TABFILE" -a $CONV -ne 0 ]; then
        rm "$TABFILE"
    fi
    exit 1
}
function die() {
    echo >&2 "$@"
    exit 1
}
HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
SCRIPTABLE=0

test $# -ge 1 || die "Usage: $0 [-s] DATAPOINTS-FILE.{dpts|rtab}"
if [ "$1" = "-s" ]; then
    SCRIPTABLE=1
    shift
fi

FILE="$1"
CONV=1
if [ "${FILE#*.}" = "rtab" ]; then
    CONV=0
    TABFILE=$FILE
else
    TABFILE=$(mktemp)
    "$HERE"/../build/dataexport "$FILE" > "$TABFILE"
fi

TMP=$(mktemp)
Rscript "$HERE"/calculate_work.R "$TABFILE" CPU 0.01 TRIGGER > "$TMP"
if [ $CONV -ne 0 ]; then
    rm "$TABFILE"
fi
if [ $SCRIPTABLE -ne 0 ]; then
    if grep -q weird -- "$TMP"; then
        rm -- "$TMP"
        exit 1
    else
        cat -- "$TMP" | grep -vE '#.*$'
    fi
else
    cat -- "$TMP"
fi
rm -- "$TMP"
exit 0
