#!/bin/bash

EV_URL="http://software.intel.com/sites/products/documentation/hpc/amplifierxe/en-us/lin/ug_docs/reference/hh_index.htm"
PFM_DIR="$HOME/libpfm-4.1.0/perf_examples"

trap unrec_err ERR
function unrec_err() {
    echo "ERROR occured"
}

function die() {
    echo "$@"
    exit 1
}

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 AVAILABLE-FILE UNAVAILABLE-FILE"
    exit 1
fi

test -d "$PFM_DIR" || die "libpfm in '$PFM_DIR' not found"

AV_F=$1
NAV_F=$2

for f in "$AV_F" "$NAV_F"; do
    if [ -f "$f" ]; then
        rm -- "$f"
    fi
    touch -- "$f"
done

NOAE=0
NOUE=0
while read ev; do
    ev=$(echo $ev | tr . :)
    if "$PFM_DIR/task" -e "$ev" true &> /dev/null; then
        let NOAE=$NOAE+1
        echo "$ev" >> "$AV_F"
    else
        let NOUE=$NOUE+1
        echo "$ev" >> "$NAV_F"
    fi
done < <(
    curl -s "$EV_URL" | grep '<option' | \
        sed -r 's/<option[0-9a-z _\."/=-]+>([A-Za-z0-9_\.]+)<\/option>/\1/g' | \
        sort | uniq
    )

echo SUCCESS: $NOAE available, $NOUE unavailable
