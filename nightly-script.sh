#!/bin/bash

> rep/json/derivable_rules/bool.json
> rep/json/derivable_rules/bv32.json
> rep/json/derivable_rules/bv4.json
> rep/json/derivable_rules/rational.json
> rep/json/output.json

> rep/index.html
> rep/bool.html
> rep/bv32.html
> rep/bv4.html
> rep/rational.html

echo -n "[" >> rep/json/output.json
cargo test
echo -n "]" >> rep/json/output.json

OUTPUT=`cat rep/json/output.json`
sed "11 i var obj = ${OUTPUT};" rep/index_base.html > rep/index.html
echo "echo ${OUTPUT}"

for FILE in rep/json/derivable_rules/*.json; do
    CONTENT=`cat ${FILE}`
    NAME=${FILE%.json}
    NEWFILE=rep/${NAME##*/}.html
    cp rep/base.html ${NEWFILE}
    sed -i "s|NAME|${NAME##*/}|g" ${NEWFILE}
    sed -i "18 i var obj = ${CONTENT};" ${NEWFILE}
done

DIR="rep"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name "$RDIR" "$DIR"