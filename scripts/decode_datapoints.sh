#!/bin/bash

function unrecoverable_err() {
    echo "UNRECOVERABLE ERROR :-("
}

function die() {
    echo "ERROR: $@"
    exit 1
}

trap unrecoverable_err ERR
set -e
HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
F="$1"

#from
function uint32_le_decode() {
    s=$(dd if="$F" bs=1 skip="$1" count=4 2> /dev/null)
    hex=0x$(printf "%s" \
          "${s:7:1}${s:6:1}${s:5:1}${s:4:1}${s:3:1}${s:2:1}${s:1:1}${s:0:1}" | \
          od -t x1 -An | tr -dc '[:alnum:]')
    python -c "print $hex"
}

#from to
function bytes() {
    raw_bytes "$1" "$2" | od -t x1 -An | tr -dc '[:alnum:]'
}

#from to
function raw_bytes() {
    dd if="$F" bs=1 skip="$1" count="$2" 2> /dev/null
}

#message name
function decode_proto () {
    protoc --decode="$1" -I"$HERE/../protos" \
        "$HERE/../protos/measured-data.proto"
}

test $# -eq 1 || die "Usage: $0 DATAPOINT-FILE"
test -f $1 || die "file '$1' does not exist"
F_SIZE=$(du -b "$F" | cut -f1)
let F_SIZE=$F_SIZE-1 #newline before eof
OFFSET=0

MAGIC=$(bytes $OFFSET 4)
let OFFSET=$OFFSET+4
test "$MAGIC" = "03018601" || die "$F: parse error: header magic bytes bad"

VERSION=$(bytes $OFFSET 1)
let OFFSET=$OFFSET+1
test "$VERSION" = "01" || die "$F: wrong version '$VERSION'"

H_LENGTH=$(uint32_le_decode $OFFSET)
let OFFSET=$OFFSET+4

echo "VERSION: $VERSION"
echo "HEADER_LENGTH: $H_LENGTH"
echo "--- BEGIN HEADER ---"
raw_bytes $OFFSET $H_LENGTH | decode_proto MeasuredData
let OFFSET=$OFFSET+$H_LENGTH
echo "--- END HEADER (OFFSET=$OFFSET, SIZE=$F_SIZE) ---"

PNO=0
while [[ $OFFSET -lt $F_SIZE ]]; do
    let PNO=$PNO+1

    MAGIC=$(bytes $OFFSET 4)
    let OFFSET=$OFFSET+4
    test "$MAGIC" = "03018602" || die "$F: part magic bytes not found "\
"(offset=$OFFSET, file size=$F_SIZE)"

    echo "--- BEGIN PART $PNO ---"
    P_LENGTH=$(uint32_le_decode $OFFSET)
    let OFFSET=$OFFSET+4
    raw_bytes $OFFSET $P_LENGTH | decode_proto DataSet
    let OFFSET=$OFFSET+$P_LENGTH
    echo "--- END PART $PNO ---"
done
echo "SUCCESS"
test $OFFSET -eq $F_SIZE || die "offset '$OFFSET' != file size '$F_SIZE'"
exit 0
