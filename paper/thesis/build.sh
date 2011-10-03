#!/bin/bash

STEP="main program"
function err() {
    echo "UNEXCEPTION ERROR (in $STEP)"
    exit 1
}

trap err ERR

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
TEXOPTS="-output-directory build -halt-on-error"
cd "$HERE"

function try_pdflatex() {
    set +e
    TMP=$(mktemp)
    pdflatex "$@" &> "$TMP"
    if [ $? -eq 0 ]; then
        rm -- "$TMP"
        set -e
        return 0
    else
        cat -- "$TMP"
        rm -- "$TMP"
        set -e
        return 1
    fi
}

./clean.sh

set -e
STEP="texification of ressources)"
../scripts/texify-verb.sh ../../protos/measured-data.proto > res/pb-dpts.tex
../scripts/texify-verb.sh ../../libdatapoints/dump-format.text > res/dpts.tex
../scripts/texify-verb.sh ../../protos/perf-counters.proto > res/pb-ctrs.tex
../scripts/texify-verb.sh ../../protos/generic.proto > res/pb-generic.tex
STEP="pdflatex run 1"
try_pdflatex $TEXOPTS thesis
STEP="bibtex"
bibtex build/thesis
STEP="pdflatex run 2"
try_pdflatex $TEXOPTS thesis
STEP="pdflatex run 3"
pdflatex $TEXOPTS -halt-on-error thesis |& tee /tmp/pdflatex.out
STEP="main program"
cp build/thesis.pdf .

echo BUILD SUCCESSFUL
#
#if pgrep -f 'evince (.+\/)?thesis.pdf' > /dev/null; then
#    echo "updating evince..."
#    evince thesis.pdf
#fi
