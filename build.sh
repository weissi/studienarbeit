#!/bin/bash

set -e

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
BUILD="$HERE/build"
CFLAGS="$CFLAGS -I$HERE/gensrc -I$HERE/libmisc -I$HERE/libdatapoints \
        -pedantic -Wall -Werror -ggdb --std=gnu99 -pg"
LDFLAGS="$LDFLAGS"
cd "$HERE"

function build() {
    BINARY="$1"
    shift
    echo "Building '$BINARY'..."
    gcc "$@" 2>&1 | while read line; do
        echo "    $line"
    done
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

build "datadump" -o $BUILD/datadump $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt -lnidaqmxbase \
    -pedantic -Wall -Werror \
    -ggdb \
    $HERE/datadump/*.c $HERE/libdatapoints/datapoints.c \
    $HERE/gensrc/*.c $HERE/libmisc/*.c

build "dataexport" --std=gnu99 -o $BUILD/dataexport $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    -pedantic -Wall -Werror \
    -ggdb \
    $HERE/dataexport/*.c $HERE/libdatapoints/datapoints.c \
    $HERE/gensrc/*.c $HERE/libmisc/*.c

echo SUCCESS
