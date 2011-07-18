#!/bin/bash

set -e
set -o pipefail

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
BUILD="$HERE/build"
CFLAGS="$CFLAGS -I$HERE/gensrc -I$HERE/libmisc -I$HERE/libdatapoints \
        -I$HERE/ctrbenchmark \
        -pedantic -pedantic-errors -Wall -Werror --std=gnu99"
LDFLAGS="$LDFLAGS"
PATH="$PATH:$HERE/build:$HOME/.cabal/bin"
cd "$HERE"

function build_raw() {
    COMPILER="$1"
    BINARY="$2"
    shift
    shift
    echo "- Building '$BINARY'..."
    $COMPILER "$@" 2>&1 | while read line; do
        echo "    $line"
    done
}

function build() {
    build_raw gcc "$@"
}

function checklib() {
    ld $LDFLAGS -l"$1"
}

echo -n "- generating prots... "
cd protos &> /dev/null
for f in *.proto; do
    if [ "$f" != "hs-perf-counters.proto" ]; then
        protoc-c --c_out=../gensrc "$f"
    fi
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

build ctrbenchmark --std=gnu99 -o $BUILD/ctrbenchmark $CFLAGS $LDFLAGS \
    -Wall -Werror -pedantic -ggdb \
    $HERE/ctrbenchmark/*.c $HERE/ctrbenchmark/benchlets/*.c

if which hprotoc &> /dev/null; then
    hprotoc -d../gensrc -I../protos ../protos/hs-perf-counters.proto > /dev/null
    build_raw ghc "BuildSLE" -Wall -o $BUILD/buildsle --make \
        -i$HERE/gensrc $HERE/buildsle/*.hs
else
    echo '- not building BuildSLE (Build System of Linear Equations): haskell'
fi

echo SUCCESS
