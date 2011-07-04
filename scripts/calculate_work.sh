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

test $# -eq 1 || die "Usage: $0 DATAPOINTS-FILE.dpts"
FILE="$1"
TABFILE=$(mktemp)
"$HERE"/../build/dataexport "$FILE" > "$TABFILE"
Rscript "$HERE"/calculate_work.R "$TABFILE" CPU TRIGGER
rm "$TABFILE"
