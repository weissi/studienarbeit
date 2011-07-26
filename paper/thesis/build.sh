#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

./clean.sh

set -e
pdflatex thesis
bibtex thesis
pdflatex thesis
pdflatex thesis

echo BUILD SUCCESSFUL

if pgrep -f 'evince (.+\/)?thesis.pdf' > /dev/null; then
    echo "updating evince..."
    evince thesis.pdf
fi
