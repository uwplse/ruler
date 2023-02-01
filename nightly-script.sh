#!/bin/bash

> rep/json/bool.json
> rep/json/bv32.json
> rep/json/bv4.json
> rep/json/rational.json
> rep/json/output.json

> rep/index.html
> rep/bool.html
> rep/bv32.html
> rep/bv4.html
> rep/rational.html

> terms.workload

echo -n "[" >> rep/json/output.json
cargo test
echo -n "]" >> rep/json/output.json

OUTPUT=`cat rep/json/output.json`
sed "8 i var obj = ${OUTPUT};" rep/index_base.html > rep/index.html
echo "echo ${OUTPUT}"

BOOL=`cat rep/json/bool.json`
sed "13 i var obj = ${BOOL};" rep/bool_base.html > rep/bool.html

BV32=`cat rep/json/bv32.json`
sed "13 i var obj = ${BV32};" rep/bv32_base.html > rep/bv32.html

BV4=`cat rep/json/bv4.json`
sed "13 i var obj = ${BV4};" rep/bv4_base.html > rep/bv4.html

RATIONAL=`cat rep/json/bool.json`
sed "13 i var obj = ${RATIONAL};" rep/rational_base.html > rep/rational.html

DIR="rep"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name "$RDIR" "$DIR"