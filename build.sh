#!/bin/bash

set -e

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
CFLAGS="$CFLAGS -I$HERE/gensrc -I$HERE/libmisc -I$HERE/libdatapoints"
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
gcc --std=gnu99 -o out $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    -pedantic -Wall -Werror \
    -ggdb \
    $HERE/libdatapoints/datapoints.c $HERE/gensrc/*.c

echo SUCCESS
