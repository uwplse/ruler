#!/bin/bash
# The primary purpose of this script is to run all the tests and upload 
# the results to the nightly-results server. It also generates some HTML
# to display the equiderivability tests in a nicely-formatted way.

rm rep/json/*
rm rep/json/derivable_rules/*

# Run tests.
cargo test --release

# Update HTML index page.
OUTPUT=`cat rep/json/output.json`
sed "11 i var obj = ${OUTPUT};" rep/index_base.html > rep/index.html
echo "echo ${OUTPUT}"

# Loop through all the files in the "derivable_rules" subdirectory
# and generate HTML files for each one that show which rules are 
# and are not derivable.
for FILE in rep/json/derivable_rules/*.json; do
    CONTENT=`cat ${FILE}`
    NAME=${FILE%.json}
    NEWFILE=rep/${NAME##*/}.html
    cp rep/base.html ${NEWFILE}
    sed -i "s|NAME|${NAME##*/}|g" ${NEWFILE}
    sed -i "18 i var obj = ${CONTENT};" ${NEWFILE}
done

# This is the uploading part, copied directly from Herbie's nightly script.
DIR="rep"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name "$RDIR" "$DIR"
