#!/bin/bash

set -e

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
CFLAGS="$CFLAGS -I$HERE/gensrc -I$HERE/libmisc -I$HERE/libdatapoints \
        -pedantic -Wall -Werror -ggdb --std=gnu99 -pg"
LDFLAGS="$LDFLAGS"
cd "$HERE"

echo -n "- generating prots... "
cd protos &> /dev/null
for f in *.proto; do
    protoc-c --c_out=../gensrc "$f"
done
cd - &> /dev/null
echo "done"

cd build
gcc -o ../libdatapoints/demo_datapoints $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    $HERE/libdatapoints/*.c $HERE/gensrc/*.c

gcc -o ../datadump/datadump $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt -lnidaqmxbase \
    $HERE/datadump/*.c $HERE/libdatapoints/datapoints.c \
    $HERE/gensrc/*.c

echo SUCCESS
