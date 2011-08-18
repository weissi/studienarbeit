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

BO=""
if [ $# -eq 1 ]; then
    BO="$1"
fi

function build_raw() {
    COMPILER="$1"
    BUILD_ONLY="$2"
    BINARY="$3"
    shift
    shift
    shift
    if [ "$BUILD_ONLY" != "" -a "$BUILD_ONLY" != "$BINARY" ]; then
        echo "- skipping '$BINARY' (not selected)"
    else
        echo "- Building '$BINARY'..."
        $COMPILER "$@" 2>&1 | while read line; do
            echo "    $line"
        done
    fi
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

build "$BO" "demo_datapoints" -o $BUILD/demo_datapoints $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    $HERE/libdatapoints/datapoints.c $HERE/gensrc/*.c $HERE/libmisc/*.c \
    $HERE/libdatapoints/demo_datapoints.c

if checklib nidaqmxbase &> /dev/null; then
    build "$BO" "datadump" -o $BUILD/datadump $CFLAGS $LDFLAGS \
        -lprotobuf -lprotobuf-c -lrt -lnidaqmxbase \
        -pedantic -Wall -Werror \
        -ggdb \
        $HERE/datadump/*.c $HERE/libdatapoints/datapoints.c \
        $HERE/gensrc/*.c $HERE/libmisc/*.c
else
    echo '- not building datadump, NIdaqMXbase not found'
fi

build "$BO" "dataexport" --std=gnu99 -o $BUILD/dataexport $CFLAGS $LDFLAGS \
    -lprotobuf -lprotobuf-c -lrt \
    -pedantic -Wall -Werror \
    -ggdb \
    $HERE/dataexport/*.c $HERE/libdatapoints/datapoints.c \
    $HERE/gensrc/*.c $HERE/libmisc/*.c

if checklib pfm &> /dev/null; then
    build "$BO" "dumpcounters" -o $BUILD/dumpcounters $CFLAGS $LDFLAGS \
        -lpfm -lprotobuf -lprotobuf-c -lrt \
        -pedantic -Wall -Werror \
        -ggdb \
        $HERE/dumpcounters/*.c \
        $HERE/gensrc/*.c $HERE/libmisc/*.c
else
    echo '- not building dumpcounters, libpfm not found'
fi

build "$BO" ctrbenchmark --std=gnu99 -o $BUILD/ctrbenchmark $CFLAGS $LDFLAGS \
    -Wall -Werror -pedantic -ggdb \
    $HERE/ctrbenchmark/*.c $HERE/ctrbenchmark/benchlets/*.c

if which hprotoc &> /dev/null; then
    if [ "$PROFILING" = "1" ]; then
        PROF_OPTS="-prof -auto-all -caf-all -fforce-recomp -rtsopts"
    else
        PROF_OPTS=""
    fi

    hprotoc -d../gensrc -I../protos ../protos/hs-perf-counters.proto > /dev/null
    build_raw ghc "$BO" "BuildSLE" -O2 -Wall -o $BUILD/buildsle \
         --make -i$HERE/gensrc $HERE/buildsle/*.hs \
         $PROF_OPTS
    echo "  (profiling opts: '$PROF_OPTS')"
else
    echo '- not building BuildSLE (Build System of Linear Equations): haskell'
fi

echo SUCCESS
