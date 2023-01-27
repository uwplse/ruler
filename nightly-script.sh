#!/bin/bash
touch output.json
echo -n "[" >> output.json
cargo test
echo -n "]" >> output.json

OUTPUT=`cat output.json`
echo 
sed -i "204 i var obj = ${OUTPUT};" rep/index.html
echo "echo ${OUTPUT}"

DIR="rep"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name
nightly-results publish --name "$RDIR" "$DIR"

rm output.json