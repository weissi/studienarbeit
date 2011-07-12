#!/bin/bash

set -e
trap err ERR
function err() {
    echo "UNRECOVERABLE ERROR"
    if [ -f "$TABFILE" ]; then
        rm "$TABFILE"
    fi
    exit 1
}
function die() {
    echo "$@"
    exit 1
}
HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)

test $# -eq 1 || die "Usage: $0 DATAPOINTS-FILE.{dpts|rtab}"
FILE="$1"
CONV=1
if [ "${FILE#*.}" = "rtab" ]; then
    CONV=0
    TABFILE=$FILE
else
    TABFILE=$(mktemp)
    "$HERE"/../build/dataexport "$FILE" > "$TABFILE"
fi

Rscript "$HERE"/calculate_work.R "$TABFILE" CPU TRIGGER
if [ $CONV -ne 0 ]; then
    rm "$TABFILE"
fi
