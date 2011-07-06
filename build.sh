#!/bin/bash

set -e
set -o pipefail

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
BUILD="$HERE/build"
CFLAGS="$CFLAGS -I$HERE/gensrc -I$HERE/libmisc -I$HERE/libdatapoints \
        -pedantic -pedantic-errors -Wall -Werror --std=gnu99"
LDFLAGS="$LDFLAGS"
cd "$HERE"

function build() {
    BINARY="$1"
    shift
    echo "- Building '$BINARY'..."
    gcc "$@" 2>&1 | while read line; do
        echo "    $line"
    done
}

function checklib() {
    ld $LDFLAGS -l"$1"
}

echo -n "- generating prots... "
cd protos &> /dev/null
for f in *.proto; do
    protoc-c --c_out=../gensrc "$f"
done
cd - &> /dev/null
echo "done"

cd build

build "demo_datapoints" -o $BUILD/demo_datapoints $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    $HERE/libdatapoints/datapoints.c $HERE/gensrc/*.c $HERE/libmisc/*.c \
    $HERE/libdatapoints/demo_datapoints.c

if checklib nidaqmxbase &> /dev/null; then
    build "datadump" -o $BUILD/datadump $CFLAGS $LDFLAGS \
        -lprotobuf -lprotobuf-c -lrt -lnidaqmxbase \
        -pedantic -Wall -Werror \
        -ggdb \
        $HERE/datadump/*.c $HERE/libdatapoints/datapoints.c \
        $HERE/gensrc/*.c $HERE/libmisc/*.c
else
    echo '- not building datadump, NIdaqMXbase not found'
fi

build "dataexport" --std=gnu99 -o $BUILD/dataexport $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    -pedantic -Wall -Werror \
    -ggdb \
    $HERE/dataexport/*.c $HERE/libdatapoints/datapoints.c \
    $HERE/gensrc/*.c $HERE/libmisc/*.c

if checklib pfm &> /dev/null; then
    build "dumpcounters" -o $BUILD/dumpcounters $CFLAGS $LDFLAGS \
        -lpfm -lprotobuf -lprotobuf-c -lrt \
        -pedantic -Wall -Werror \
        -ggdb \
        $HERE/dumpcounters/*.c \
        $HERE/gensrc/*.c $HERE/libmisc/*.c
else
    echo '- not building dumpcounters, libpfm not found'
fi

echo SUCCESS
