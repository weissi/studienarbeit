#!/bin/bash

EV_URL="http://software.intel.com/sites/products/documentation/hpc/amplifierxe/en-us/lin/ug_docs/reference/hh_toc.htm"
PFM_DIR="/home/weiss/libpfm-4.1.0/perf_examples"

trap unrec_err ERR
function unrec_err() {
    echo "ERROR occured"
}

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 AVAILABLE-FILE UNAVAILABLE-FILE"
    exit 1
fi

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
while read arch ev; do
    ev=$(echo $ev | tr . : | tr a-z A-Z)
    if "$PFM_DIR/task" -e $ev true &> /dev/null; then
        let NOAE=$NOAE+1
        echo "$arch/$ev" >> "$AV_F"
    else
        let NOUE=$NOUE+1
        echo "$arch/$ev" >> "$NAV_F"
    fi
done < <(
    curl "$EV_URL" |& grep -Eio '"[a-z]+/events/[a-z0-9_.]+' |
        sed -r 's#"([a-z]+)/events/(.*)\.html#\1 \2#g'
)

echo SUCCESS: $NOAE available, $NOUE unavailable
