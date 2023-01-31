#!/bin/bash
touch rep/output.json
touch rep/index.html
echo -n "[" >> rep/output.json
cargo test
echo -n "]" >> rep/output.json

OUTPUT=`cat rep/output.json`
echo 
sed "8 i var obj = ${OUTPUT};" rep/index_base.html > rep/index.html
echo "echo ${OUTPUT}"

DIR="rep"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name "$RDIR" "$DIR"

rm rep/output.json