#!/bin/bash
touch output.json
echo -n "[" >> output.json
cargo test
echo -n "]" >> output.json

OUTPUT=`cat output.json`
sed -i "204 i var obj = ${OUTPUT};" index.html

nightly-results publish --name output.json output.json
nightly-results publish --name index.html index.html

rm output.json