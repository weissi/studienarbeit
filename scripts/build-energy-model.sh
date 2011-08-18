#!/bin/bash

set -e

trap err ERR

function err() {
    echo "UNRECOVERABLE ERROR"
    exit 1
}

function usage() {
    echo "Usage: $0 MODEL-TABLE-FILE"
}

test $# -eq 1 || { usage; exit 1; }
F="$1"

FORMULA=$(cat -- "$F" | \
          sed -e1d -e's/"(Intercept)" //g' -e's/^"/tabledata$/g' -e's/"//g' |\
          tr " \n" "*+" | sed -r 's/\+$//g')
echo "model <- function(tabledata) $FORMULA"
