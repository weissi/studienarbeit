#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
TEXOPTS="-interaction=batchmode -halt-on-error"
cd "$HERE"

./clean.sh

set -e
pdflatex $TEXOPTS thesis
bibtex thesis
pdflatex $TEXOPTS thesis
pdflatex $TEXOPTS thesis

echo BUILD SUCCESSFUL

if pgrep -f 'evince (.+\/)?thesis.pdf' > /dev/null; then
    echo "updating evince..."
    evince thesis.pdf
fi
