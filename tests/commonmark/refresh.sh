#!/bin/bash

export CURR=`pwd`
DIR=`mktemp -d`
git clone https://github.com/commonmark/commonmark $DIR
cd $DIR

python3 test/spec_tests.py -d |
    jq '.[] | .example, .markdown' |
    while read N
    do
        N=$(printf "%03d" $N)
        IFS='' read -r line
        echo "$line" | jq -r '. | @base64' | base64 --decode -o "$CURR/cmark-$N.md"
    done

python3 test/spec_tests.py -d |
    jq '.[] | .example, .html' |
    while read N
    do
        N=$(printf "%03d" $N)
        IFS='' read -r line
        echo "$line" | jq -r '. | @base64' | base64 --decode -o "$CURR/cmark-$N.html"
    done
