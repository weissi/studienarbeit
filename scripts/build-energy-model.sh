#!/bin/bash

set -e

trap err ERR

function err() {
    echo "UNRECOVERABLE ERROR"
    exit 1
}

function usage() {
    echo "Usage: $0 [-R|-C|-H] MODEL-TABLE-FILE"
    echo
    echo "-R: R only"
    echo "-C: Counters only"
    echo "-H: Human redable"
    echo "-E: Event list string"
}

VERBOSE=1
DO_R=1
DO_C=1
DO_H=1
DO_E=1
case "$1" in
    "-R")
        DO_C=0
        DO_H=0
        DO_E=0
        VERBOSE=0
        shift
        ;;
    "-C")
        DO_R=0
        DO_H=0
        DO_E=0
        VERBOSE=0
        shift
        ;;
    "-H")
        DO_R=0
        DO_C=0
        DO_E=0
        VERBOSE=0
        shift
        ;;
    "-E")
        DO_R=0
        DO_C=0
        DO_H=0
        VERBOSE=0
        shift
        ;;
    *)
        ;;
esac

test $# -eq 1 || { usage; exit 1; }
F="$1"

FORMULA=$(cat -- "$F" | \
          sed -e1d -e's/"(Intercept)" //g' -e's/^"/tabledata$/g' -e's/"//g' |\
          tr " \n" "*+" | sed -r 's/\+$//g')
test $VERBOSE -eq 1 && { echo "#!/usr/bin/R"; }
test $DO_R -eq 1    && {
    echo "model <- function(tabledata) $FORMULA";
    echo 'model_rel <- function(tabledata) data.frame('"$(
        echo "$FORMULA" | sed 's#+#/tabledata$WORK, #g')"'/tabledata$WORK)'
}
test $VERBOSE -eq 1 && { echo; echo "# COUNTERS IN MODEL"; }
test $DO_C -eq 1    && { sed 1,3d "$F" | cut -d'"' -f2 | tr . :; }
test $VERBOSE -eq 1 && { echo; echo "# HUMAN READABLE"; }
test $DO_H -eq 1    && { cat -- "$F" | \
    sed -e1d -e's/"(Intercept)" //g' -e's/"//g'  | \
    awk -F'[ ]' '{ print $2, "\t", $1 }'; }
test $VERBOSE -eq 1 && { echo; echo "# EVENT LIST STRING"; }
test $DO_E -eq 1    && { sed 1,3d "$F" | cut -d'"' -f2 | tr . : | \
                         tr "\n" , | sed -r 's/^(.*),$/\1/g'; echo; }
