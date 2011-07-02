#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)

cat "$1" | protoc -I "$HERE/../protos" --decode=CounterData \
    "$HERE/../protos/perf-counters.proto"
