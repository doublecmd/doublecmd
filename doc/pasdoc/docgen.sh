#!/bin/sh
# https://pasdoc.github.io

mkdir -p html
pasdoc --marker en --output html --define UNIX --source units-doc-unix.txt
